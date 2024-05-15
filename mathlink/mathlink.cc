#include "wstp.h"
#include "perm.h"
#include <vector>
#include <fstream>
#include <cstdlib>
#include <string>

using namespace pperm;

#define PREFIX "Peanotica`Perm`"
#define P_PREFIX "Peanotica`Perm`Private`"
#define TRY(fn) if(!(fn)) return false;

int WSDone = 0, WSAbort = 0;

WSMDEFN(void, WSDefaultHandler, (WSLINK wslp, int message, int n));

static bool readPermutation(WSLINK link, PermutationView perm) {
    int len2;
    TRY(WSTestHead(link, "List", &len2));
    TRY(len2 >= 1 && len2 <= perm.len + 1);
    int sign;
    TRY(WSGetInteger32(link, &sign));
    perm.identity();
    if (sign == -1) {
        perm.setNegative(true);
    } else {
        TRY(sign == 1);
        perm.setNegative(false);
    }
    auto images = perm.images();
    for (std::size_t i = 0; i < len2 - 1; i++, images++) {
        int val;
        TRY(WSGetInteger32(link, &val));
        TRY(val >= 1 && val <= perm.len);
        *images = upoint_type(val - 1);
    }
    return true;
}

static bool readPermutationList(WSLINK link, PermutationList &list) {
    int len;
    TRY(WSTestHead(link, "List", &len));
    for (int i = 0; i < len; i++) {
        TRY(readPermutation(link, list.push().identity()));
    }
    return true;
}

static bool writePermutation(WSLINK link, PermutationView perm) {
    TRY(WSPutFunction(link, "List", perm.len + 1));
    if (perm.isNegative()) {
        TRY(WSPutInteger32(link, -1));
    } else {
        TRY(WSPutInteger32(link, 1));
    }
    auto ptr = perm.images();
    for (std::size_t i = 0; i < perm.len; i++, ptr++) {
        TRY(WSPutInteger64(link, *ptr + 1));
    }
    return true;
}

static bool writePermutationList(WSLINK link, PermutationList &list) {
    TRY(WSPutFunction(link, "List", list.getSize()));
    for (auto perm : list) {
        TRY(writePermutation(link, perm));
    }
    return true;
}

namespace {
    struct WSString {
        const char *str = nullptr;
        WSLINK link;
        WSString() = default;
        ~WSString() {
            if (this->str) {
                WSReleaseString(this->link, this->str);
            }
        }
        bool init(WSLINK link) {
            this->link = link;
            return WSGetString(link, &this->str);
        }
    };
    struct WSTPEnv {
        WSLINK stdlink = nullptr;
        WSEnvironment stdenv = nullptr;
        std::fstream logFile;
        bool enableLog = false;

        using Func = bool(WSTPEnv &);
        struct FunctionEntry {
            const char *pattern;
            const char *args;
            Func *callable;
        };
        bool initialize(char **argv, char **argv_end, char *commandline) {
            if (!this->stdenv) {
                this->stdenv = WSInitialize((WSEnvironmentParameter)nullptr);
            }
            int err;
            this->stdlink = commandline ? WSOpenString(stdenv, commandline, &err) : WSOpenArgcArgv(stdenv, argv_end - argv, argv, &err);
            if(this->stdlink == nullptr) {
                WSAlert(stdenv, WSErrorString(stdenv, err));
                return false;
            }
            TRY(WSSetMessageHandler(this->stdlink, WSDefaultHandler));
            WSSetUserData(this->stdlink, static_cast<void *>(this), nullptr);

            return true;
        };
        void mainLoop() {
            wsapi_packet pkt = 0;

            this->defineFunctions();
            while(!WSDone && !WSError(this->stdlink)
                && WSWAITSUCCESS == WSWaitForLinkActivity(this->stdlink) && (pkt = WSNextPacket(this->stdlink))
                && pkt == CALLPKT
            ){
                WSAbort = 0;
                if(!this->doCallPacket()) pkt = 0;
            }
            WSAbort = 0;
        }
        void registerFunction(const char *pattern, const char *args, Func *func) {
            this->functions.push_back(FunctionEntry{pattern, args, func});
        }
        ~WSTPEnv() {
            if (this->stdlink) {
                WSClose(this->stdlink);
                this->stdlink = nullptr;
            }
            if (this->stdenv) {
                WSDeinitialize(this->stdenv);
                this->stdenv = nullptr;
            }
        }
        private:
        std::vector<FunctionEntry> functions;

        bool definePattern(const char *patt, const char *args, int func_n) const {
            TRY(WSPutFunction(this->stdlink, "DefineExternal", 3));
            TRY(WSPutString(this->stdlink, patt));
            TRY(WSPutString(this->stdlink, args));
            TRY(WSPutInteger(this->stdlink, func_n));
            return true;
        }
        bool defineFunctions() const {
            TRY(WSConnect(this->stdlink));
            auto ptr = this->functions.data();
            auto size = this->functions.size();
            for (int i = 0; i < size; i++, ptr++) {
                TRY(this->definePattern(ptr->pattern, ptr->args, i));
            }
            TRY(WSPutSymbol(this->stdlink, "End"));
            TRY(WSFlush(this->stdlink));
            return true;
        }
        bool doCallPacket() {
            auto inner = [this]() -> bool {
                int n, len;
                TRY(WSGetInteger32(this->stdlink, &n));
                TRY(n >= 0 && n < this->functions.size());
                TRY(WSTestHead(this->stdlink, "List", &len));
                TRY(this->functions[n].callable(*this));
                return true;
            };
            if (!inner()) {
                TRY(WSClearError(this->stdlink));
                TRY(WSPutSymbol(this->stdlink, "$Failed"));
            }
            TRY(WSEndPacket(this->stdlink));
            TRY(WSNewPacket(this->stdlink));
            return true;
        }
    };
}

static void registerFunctions(WSTPEnv &env) {
    env.registerFunction(
        P_PREFIX "CheckLink[]",
        "{}",
        [](WSTPEnv &env) -> bool {
            TRY(WSNewPacket(env.stdlink));
            TRY(WSPutSymbol(env.stdlink, "True"));
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkConstructStrongGenSet[" P_PREFIX "GS_List, " P_PREFIX "n_Integer]",
        "{" P_PREFIX "n, " P_PREFIX "GS}",
        [](WSTPEnv &env) -> bool {
            int permLen;
            TRY(WSGetInteger32(env.stdlink, &permLen));
            PermutationList genset(permLen);
            TRY(readPermutationList(env.stdlink, genset));
            JerrumBranchingBuilder builder;
            PermutationStack stack(permLen * 16);
            builder.build(stack, genset);
            genset.clear();
            builder.branching.collectLabels([&genset](PermutationView perm){ genset.addPermutation(perm); });

            TRY(WSNewPacket(env.stdlink));
            TRY(writePermutationList(env.stdlink, genset));
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkDoubleCosetRepresentative[" P_PREFIX "S_List, " P_PREFIX "g_List, " P_PREFIX "D_List, " P_PREFIX "n_Integer, " P_PREFIX "useTwoStep_Integer, " P_PREFIX "verbose_Integer]",
        "{" P_PREFIX "n, " P_PREFIX "S, " P_PREFIX "g, " P_PREFIX "D, " P_PREFIX "useTwoStep, " P_PREFIX "verbose}",
        [](WSTPEnv &env) -> bool {
            int permLen;
            TRY(WSGetInteger32(env.stdlink, &permLen));
            PermutationList gensetS(permLen), gensetD(permLen);
            PermutationStack stack(permLen * 8);
            auto perm = stack.pushStacked(permLen);
            TRY(readPermutationList(env.stdlink, gensetS));
            TRY(readPermutation(env.stdlink, perm));
            TRY(readPermutationList(env.stdlink, gensetD));
            int twoStep, verbose;
            TRY(WSGetInteger32(env.stdlink, &twoStep));
            TRY(WSGetInteger32(env.stdlink, &verbose));

            BaseChanger baseChanger;
            BaseChangingStrongGenSetProvider gensetSProvider(baseChanger), gensetDProvider(baseChanger);
            gensetSProvider.setSGS(gensetS);
            gensetDProvider.setSGS(gensetD);

            DoubleCosetRepresentativeSolver solver;
            if (verbose && env.logFile.is_open()) {
                solver.log = &env.logFile;
                solver.permFormatter.useCycles = true;
            }
            solver.useTwoStep = bool(twoStep);
            auto ret = solver.solve(gensetSProvider, gensetDProvider, perm);
            TRY(WSNewPacket(env.stdlink));
            if (ret.has_value()) {
                TRY(writePermutation(env.stdlink, *ret));
            } else {
                TRY(WSPutInteger32(env.stdlink, 0));
            }
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkGroupOrderFromStrongGenSet[" P_PREFIX "G_List, " P_PREFIX "n_Integer]",
        "{" P_PREFIX "n, " P_PREFIX "G}",
        [](WSTPEnv &env) -> bool {
            int permLen;
            TRY(WSGetInteger32(env.stdlink, &permLen));
            PermutationList genset(permLen);
            TRY(readPermutationList(env.stdlink, genset));

            GroupOrderCalculator calc;
            calc.setGroup(genset);
            TRY(WSNewPacket(env.stdlink));
            TRY(WSPutFunction(env.stdlink, "Times", permLen));
            for (int i = 0; i < permLen; i++) {
                TRY(WSPutInteger64(env.stdlink, calc.nextFactor()));
            }
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkMoveBasePoint[" P_PREFIX "base_List, " P_PREFIX "S_List, " P_PREFIX "pos_Integer, " P_PREFIX "n_Integer]",
        "{" P_PREFIX "n, " P_PREFIX "base, " P_PREFIX "S, " P_PREFIX "pos}",
        [](WSTPEnv &env) -> bool {
            int permLen;
            TRY(WSGetInteger32(env.stdlink, &permLen));
            Array<upoint_type> base;
            int baseLen;
            TRY(WSTestHead(env.stdlink, "List", &baseLen));
            base.ensureSize(baseLen);
            for (int i = 0; i < baseLen; i++) {
                int val;
                TRY(WSGetInteger32(env.stdlink, &val));
                TRY(val > 0 && val <= permLen);
                base.get()[i] = val - 1;
            }
            PermutationList genset(permLen);
            TRY(readPermutationList(env.stdlink, genset));
            int pos;
            TRY(WSGetInteger32(env.stdlink, &pos));
            TRY(pos > 0 && pos + 1 <= baseLen);
            pos--;

            PermutationStack stack(permLen * 8);
            BaseChanger changer;
            std::size_t baseLen2 = baseLen;
            changer.setSGS(genset);
            changer.moveToFirstDirectly(MutableSlice(base.get(), baseLen2), pos, stack);

            TRY(WSNewPacket(env.stdlink));
            TRY(writePermutationList(env.stdlink, changer.genset.permutations));
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkPPermGroupElements[" P_PREFIX "genset_List, " P_PREFIX "n_Integer]",
        "{" P_PREFIX "n, " P_PREFIX "genset}",
        [](WSTPEnv &env) -> bool {
            int permLen;
            TRY(WSGetInteger32(env.stdlink, &permLen));
            PermutationStack stack(16 * permLen);
            GroupEnumerator enumerator;
            enumerator.permStack = &stack;
            enumerator.generators.setPermutationLength(permLen);
            TRY(readPermutationList(env.stdlink, enumerator.generators));
            enumerator.generate();

            TRY(WSNewPacket(env.stdlink));
            TRY(writePermutationList(env.stdlink, enumerator.elements.permutations));
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkPPermStabilizer[" P_PREFIX "n_Integer, " P_PREFIX "genset_List, " P_PREFIX "point_Integer]",
        "{" P_PREFIX "n, " P_PREFIX "genset, " P_PREFIX "point}",
        [](WSTPEnv &env) -> bool {
            int permLen;
            TRY(WSGetInteger32(env.stdlink, &permLen));
            PermutationList genset(permLen);
            TRY(readPermutationList(env.stdlink, genset));
            int point;
            TRY(WSGetInteger32(env.stdlink, &point));
            TRY(point >= 1 && point <= permLen);

            PermutationStack stack(permLen * 16);
            SchreierOrbit orbit(permLen);
            std::deque<upoint_type> queue;
            JerrumBranching sifter(permLen);
            orbit.appendOrbit(point - 1, genset, queue);
            schreierGenerators([&](PermutationView perm) {
                sifter.siftElement(stack, queue, perm, None{});
            }, stack, genset, orbit);

            PermutationList output(permLen);
            sifter.collectLabels([&output](PermutationView perm) { output.addPermutation(perm); });

            TRY(WSNewPacket(env.stdlink));
            TRY(writePermutationList(env.stdlink, output));
            return true;
        }
    );
    env.registerFunction(
        P_PREFIX "MathLinkOpenLogFile[" P_PREFIX "path_String]",
        "{" P_PREFIX "path}",
        [](WSTPEnv &env) -> bool {
            WSString path;
            TRY(path.init(env.stdlink));
            if (env.logFile.is_open()) {
                env.logFile.close();
            }
            env.logFile.open(path.str, std::ios_base::app);
            TRY(WSNewPacket(env.stdlink));
            TRY(WSPutSymbol(env.stdlink, "True"));
            return true;
        }
    );
    env.registerFunction(
        PREFIX "PPermCloseLogFile[]",
        "{}",
        [](WSTPEnv &env) -> bool {
            env.logFile.close();
            TRY(WSNewPacket(env.stdlink));
            TRY(WSPutSymbol(env.stdlink, "True"));
            return true;
        }
    );
}

WSMDEFN(void, WSDefaultHandler, (WSLINK wslp, int message, int n)) {
    WSTPEnv &env = *static_cast<WSTPEnv *>(WSUserData(wslp, nullptr));
    switch (message) {
        case WSTerminateMessage:
            WSDone = 1;
            if (env.logFile.is_open()) {
                env.logFile << "terminated message received" << std::endl;
            }
        case WSInterruptMessage:
        case WSAbortMessage:
            WSAbort = 1;
            if (env.logFile.is_open()) env.logFile << "aborting message received, exiting" << std::endl;
            std::exit(0);
        default:
            return;
    }
}

int main(int argc, char *args[]) {
    WSTPEnv env;
    if (env.initialize(args, args + argc, nullptr)) {
        registerFunctions(env);
        env.mainLoop();
    }

    return 0;
}