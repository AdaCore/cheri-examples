extern void *__builtin_cheri_global_data_get(void);
extern void *__builtin_cheri_address_set(void *, long unsigned);
extern char __heap_start;

void malicious_lib(void)
{
    void *ddc = __builtin_cheri_global_data_get();
    char *heap = __builtin_cheri_address_set(ddc, (long unsigned)&__heap_start);
    for (int i = 0; i < 1024 * 1024; i++) {
        heap[i] = 'A';
    }
}
