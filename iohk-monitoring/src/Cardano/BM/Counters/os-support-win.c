#include <windows.h>
#include <psapi.h>

#include "os-support-win.h"


/* c_get_process_memory_info */

int c_get_process_memory_info (PROCESS_MEMORY_COUNTERS *counters, DWORD pid) {
    HANDLE hProc;
    hProc = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
                        FALSE, pid );
    if (NULL == hProc) { return -2; }
    BOOL result = GetProcessMemoryInfo(hProc, counters, sizeof(PROCESS_MEMORY_COUNTERS));
    CloseHandle(hProc);
    return result;
}


/* c_get_io_counters */

int c_get_io_counters (IO_COUNTERS *counters, DWORD pid) {
    HANDLE hProc;
    hProc = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
                        FALSE, pid );
    if (NULL == hProc) { return -2; }
    BOOL result = GetProcessIoCounters(hProc, counters);
    CloseHandle(hProc);
    return result;
}


// defined in 'psutil'
    #define LO_T 1e-7
    #define HI_T 429.4967296

/* c_get_sys_cpu_times */
int c_get_sys_cpu_times (CPU_TIMES *cputimes) {
    FILETIME usert={0,0}, kernelt={0,0}, idlet={0,0};
    if (! GetSystemTimes(&idlet, &kernelt, &usert) ) {
      return -2;
    }
    cputimes->usertime = (ULONGLONG)(HI_T * usert.dwHighDateTime + LO_T * usert.dwLowDateTime)/*  * 1e9 */;
    ULONGLONG kerneltime = (ULONGLONG)(HI_T * kernelt.dwHighDateTime + LO_T * kernelt.dwLowDateTime)/*  * 1e9 */;
    cputimes->idletime = (ULONGLONG)(HI_T * idlet.dwHighDateTime + LO_T * idlet.dwLowDateTime)/*  * 1e9 */;
    cputimes->systime = kerneltime - cputimes->idletime;
    return 1;
}

/* c_get_proc_cpu_times */
int c_get_proc_cpu_times (CPU_TIMES *cputimes, DWORD pid) {
    HANDLE hProc;
    hProc = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
                        FALSE, pid );
    if (NULL == hProc) { return -2; }
    FILETIME usert={0,0}, kernelt={0,0}, createt={0,0}, exitt={0,0};
    if (! GetProcessTimes(hProc, &createt, &exitt, &kernelt, &usert) ) {
      return -1;
    }
    CloseHandle(hProc);
    cputimes->usertime = (ULONGLONG)(HI_T * usert.dwHighDateTime + LO_T * usert.dwLowDateTime)/*  * 1e9 */;
    cputimes->systime = (ULONGLONG)(HI_T * kernelt.dwHighDateTime + LO_T * kernelt.dwLowDateTime)/*  * 1e9 */;
    // return time since process start in "idletime"
    cputimes->idletime = (ULONGLONG)(HI_T * createt.dwHighDateTime + LO_T * createt.dwLowDateTime)/*  * 1e9 */;
    return 1;
}

/* c_get_system_info */
int c_get_system_info (SYSTEM_INFO *sysinfo) {
    GetSystemInfo (sysinfo);
    return 1;
}

/* c_get_win_bits */
int c_get_win_bits (DWORD pid) {
    BOOL res;
    HANDLE hProc;
    hProc = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
                        FALSE, pid );
    if (NULL == hProc) { return -2; }
    if (! IsWow64Process(hProc, &res)) {
      return -1;
    }
    CloseHandle(hProc);
    if (res) {
        return 32;
    }
    return 64;
}

