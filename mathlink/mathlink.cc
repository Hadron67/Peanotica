#include <mathlink.h>
#include "perm.h"

int main(int argc, char *args[]) {
    MLENV env;
    MLINK mlink;

    int err;

    if (!(env = MLInitialize(0))) {
        return -1;
    }
    if (!(mlink = MLOpenArgcArgv(0, argc, args, &err))) {
        MLDeinitialize(env);
        return -1;
    }
    return 0;
}