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

WSMDEFN(void, WSDefaultHandler, (WSLINK wslp, int message, int n)) {
    switch (message){
        case WSTerminateMessage:
            WSDone = 1;
        case WSInterruptMessage:
        case WSAbortMessage:
            WSAbort = 1;
        default:
            return;
    }
}

static bool readPermutation(WSLINK link, PermutationView perm) {
    int len2;
    TRY(WSTestHead(link, "List", &len2));
    TRY(perm.len + 1 == len2);
    int sign;
    TRY(WSGetInteger32(link, &sign));
    if (sign == -1) {
        perm.setNegative(true);
    } else {
        TRY(sign == 1);
    }
    auto images = perm.images();
    for (std::size_t i = 0; i < perm.len; i++, images++) {
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
    WSPutFunction(link, "List", perm.len + 1);
    if (perm.isNegative()) {
        WSPutInteger32(link, -1);
    } else {
        WSPutInteger32(link, 1);
    }
    auto ptr = perm.images();
    for (std::size_t i = 0; i < perm.len; i++, ptr++) {
        WSPutInteger64(link, *ptr + 1);
    }
}

static bool writePermutationList(WSLINK link, PermutationList &list) {
    WSPutFunction(link, "List", list.getSize());
    for (auto perm : list) {
        writePermutation(link, perm);
    }
    return true;
}

namespace {
    struct WSInterger32List {
        WSLINK link;
        int *data = nullptr;
        int len = 0;
        WSInterger32List() = default;
        WSInterger32List(const WSInterger32List &) = delete;
        WSInterger32List(WSInterger32List &&other) {
            this->link = other.link;
            this->data = other.data;
            this->len = other.len;
            other.data = nullptr;
            other.len = 0;
        };
        bool init(WSLINK link) {
            return WSGetInteger32List(link, &this->data, &this->len);
        }
        ~WSInterger32List() {
            if (this->data) {
                WSReleaseInteger32List(this->link, this->data, this->len);
            }
        }
    };
    struct WSInterger64List {
        WSLINK link;
        wsint64 *data = nullptr;
        int len = 0;
        WSInterger64List() = default;
        WSInterger64List(const WSInterger64List &) = delete;
        WSInterger64List(WSInterger64List &&other) {
            this->link = other.link;
            this->data = other.data;
            this->len = other.len;
            other.data = nullptr;
            other.len = 0;
        };
        bool init(WSLINK link) {
            return WSGetInteger64List(link, &this->data, &this->len);
        }
        ~WSInterger64List() {
            if (this->data) {
                WSReleaseInteger64List(this->link, this->data, this->len);
            }
        }
    };

    struct WSTPEnv {
        WSLINK stdlink = nullptr;
        WSEnvironment stdenv = nullptr;
        std::fstream logFile;

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
            if(this->stdlink == nullptr){
                WSAlert(stdenv, WSErrorString( stdenv, err));
                return false;
            }

            const char *logFileName = std::getenv("PPERM_LOG_FILE");
            if (logFileName == nullptr) {
                logFileName = "ppermlog.txt";
            }
            this->logFile.open(logFileName, std::ios_base::app);
            return true;
        };
        void mainLoop() {
            wsapi_packet pkt = 0;
            int waitResult;

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
        WSYieldFunctionObject stdyielder = (WSYieldFunctionObject)nullptr;
        WSMessageHandlerObject stdhandler = (WSMessageHandlerObject)nullptr;
        std::vector<FunctionEntry> functions;

        bool definePattern(const char *patt, const char *args, int func_n) const {
            WSPutFunction(this->stdlink, "DefineExternal", (long)3);
            WSPutString(this->stdlink, patt);
            WSPutString(this->stdlink, args);
            WSPutInteger(this->stdlink, func_n);
            return !WSError(this->stdlink);
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
        PREFIX "MathLinkConstructStrongGenSet[" P_PREFIX "GS_List, " P_PREFIX "n_Integer]",
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
        PREFIX "TestFunction[" P_PREFIX "n_]",
        "{" P_PREFIX "n}",
        [](WSTPEnv &env) -> bool {
            int val;
            TRY(WSGetInteger32(env.stdlink, &val));

            TRY(WSNewPacket(env.stdlink));
            TRY(WSPutFunction(env.stdlink, "List", 1));
            TRY(WSPutInteger32(env.stdlink, val + 1));
            return true;
        }
    );
}

int main(int argc, char *args[]) {
    WSTPEnv env;
    if (env.initialize(args, args + argc, nullptr)) {
        registerFunctions(env);
        env.logFile << "successfully initialized" << std::endl;
        env.mainLoop();
    }

    return 0;
}