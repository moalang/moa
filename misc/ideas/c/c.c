#include <stdio.h>
#include "libtcc.h"  // libtcc-dev

const char *c_code = "#include <stdio.h>\nint add(int a, int b) { puts(\"hi\"); return a + b; }";

int main() {
    TCCState *state;
    int (*add_func)(int, int);
    state = tcc_new();
    if (!state) {
        fprintf(stderr, "Failed to create TCC state.\n");
        return 1;
    }
    tcc_set_output_type(state, TCC_OUTPUT_MEMORY);
    if (tcc_compile_string(state, c_code) == -1) {
        fprintf(stderr, "Failed to compile code.\n");
        tcc_delete(state);
        return 1;
    }
    if (tcc_relocate(state, TCC_RELOCATE_AUTO) < 0) {
        fprintf(stderr, "Failed to relocate code.\n");
        tcc_delete(state);
        return 1;
    }
    add_func = (int (*)(int, int))tcc_get_symbol(state, "add");
    if (!add_func) {
        fprintf(stderr, "Failed to get symbol.\n");
        tcc_delete(state);
        return 1;
    }
    int result = add_func(3, 4);
    printf("Result of add(3, 4): %d\n", result);
    tcc_delete(state);
    return 0;
}
