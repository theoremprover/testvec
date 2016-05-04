# 1 "./vfprintf.c"
# 1 "<command-line>"
# 1 "./vfprintf.c"
# 125 "./vfprintf.c"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/newlib.h" 1
# 126 "./vfprintf.c" 2
# 151 "./vfprintf.c"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/_ansi.h" 1
# 16 "/root/testvec/newlib-1.18.0/newlib/libc/include/_ansi.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/config.h" 1



# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/ieeefp.h" 1
# 5 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/config.h" 2
# 17 "/root/testvec/newlib-1.18.0/newlib/libc/include/_ansi.h" 2
# 152 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/reent.h" 1
# 93 "/root/testvec/newlib-1.18.0/newlib/libc/include/reent.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h" 1
# 14 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/_types.h" 1
# 12 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/_types.h"
typedef long _off_t;
__extension__ typedef long long _off64_t;

typedef long _fpos_t;
__extension__ typedef long long _fpos64_t;


typedef int _ssize_t;





# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 354 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 3 4
typedef unsigned int wint_t;
# 26 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/_types.h" 2


typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    unsigned char __wchb[4];
  } __value;
} _mbstate_t;

struct __flock_mutex_t_tmp;
typedef struct
{
  int __a;
  int __b;
  struct
  {
    long int __c1;
    int __c2;
  } __c;
  int __d;
  struct __flock_mutex_t_tmp * __e;
} __flock_mutex_t;

typedef struct { __flock_mutex_t mutex; } _flock_t;
# 15 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h" 2






typedef unsigned long __ULong;
# 37 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct _reent;






struct _Bigint
{
  struct _Bigint *_next;
  int _k, _maxwds, _sign, _wds;
  __ULong _x[1];
};


struct __tm
{
  int __tm_sec;
  int __tm_min;
  int __tm_hour;
  int __tm_mday;
  int __tm_mon;
  int __tm_year;
  int __tm_wday;
  int __tm_yday;
  int __tm_isdst;
};







struct _on_exit_args {
 void * _fnargs[32];
 void * _dso_handle[32];

 __ULong _fntypes;


 __ULong _is_cxa;
};
# 89 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct _atexit {
 struct _atexit *_next;
 int _ind;

 void (*_fns[32])(void);
        struct _on_exit_args _on_exit_args;
};
# 105 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct __sbuf {
 unsigned char *_base;
 int _size;
};
# 169 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct __sFILE {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;






  void * _cookie;

  _ssize_t (* _read) (struct _reent *, void *, char *, int)
                     ;
  _ssize_t (* _write) (struct _reent *, void *, const char *, int)
                            ;
  _fpos_t (* _seek) (struct _reent *, void *, _fpos_t, int);
  int (* _close) (struct _reent *, void *);


  struct __sbuf _ub;
  unsigned char *_up;
  int _ur;


  unsigned char _ubuf[3];
  unsigned char _nbuf[1];


  struct __sbuf _lb;


  int _blksize;
  int _offset;


  struct _reent *_data;



  _flock_t _lock;

  _mbstate_t _mbstate;
  int _flags2;
};
# 226 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct __sFILE64 {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;

  struct _reent *_data;


  void * _cookie;

  _ssize_t (* _read) (struct _reent *, void *, char *, int)
                     ;
  _ssize_t (* _write) (struct _reent *, void *, const char *, int)
                            ;
  _fpos_t (* _seek) (struct _reent *, void *, _fpos_t, int);
  int (* _close) (struct _reent *, void *);


  struct __sbuf _ub;
  unsigned char *_up;
  int _ur;


  unsigned char _ubuf[3];
  unsigned char _nbuf[1];


  struct __sbuf _lb;


  int _blksize;
  int _flags2;

  _off64_t _offset;
  _fpos64_t (* _seek64) (struct _reent *, void *, _fpos64_t, int);


  _flock_t _lock;

  _mbstate_t _mbstate;
};
typedef struct __sFILE64 __FILE;





struct _glue
{
  struct _glue *_next;
  int _niobs;
  __FILE *_iobs;
};
# 305 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct _rand48 {
  unsigned short _seed[3];
  unsigned short _mult[3];
  unsigned short _add;




};
# 579 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
struct _reent
{
  int _errno;




  __FILE *_stdin, *_stdout, *_stderr;

  int _inc;
  char _emergency[25];

  int _current_category;
  const char *_current_locale;

  int __sdidinit;

  void (* __cleanup) (struct _reent *);


  struct _Bigint *_result;
  int _result_k;
  struct _Bigint *_p5s;
  struct _Bigint **_freelist;


  int _cvtlen;
  char *_cvtbuf;

  union
    {
      struct
        {
          unsigned int _unused_rand;
          char * _strtok_last;
          char _asctime_buf[26];
          struct __tm _localtime_buf;
          int _gamma_signgam;
          __extension__ unsigned long long _rand_next;
          struct _rand48 _r48;
          _mbstate_t _mblen_state;
          _mbstate_t _mbtowc_state;
          _mbstate_t _wctomb_state;
          char _l64a_buf[8];
          char _signal_buf[24];
          int _getdate_err;
          _mbstate_t _mbrlen_state;
          _mbstate_t _mbrtowc_state;
          _mbstate_t _mbsrtowcs_state;
          _mbstate_t _wcrtomb_state;
          _mbstate_t _wcsrtombs_state;
   int _h_errno;
        } _reent;



      struct
        {

          unsigned char * _nextf[30];
          unsigned int _nmalloc[30];
        } _unused;
    } _new;


  struct _atexit *_atexit;
  struct _atexit _atexit0;


  void (**(_sig_func))(int);




  struct _glue __sglue;
  __FILE __sf[3];
};
# 817 "/root/testvec/newlib-1.18.0/newlib/libc/include/sys/reent.h"
extern struct _reent *_impure_ptr ;
extern struct _reent *const _global_impure_ptr ;

void _reclaim_reent (struct _reent *);







  struct _reent * __getreent (void);
# 94 "/root/testvec/newlib-1.18.0/newlib/libc/include/reent.h" 2

# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/types.h" 1
# 96 "/root/testvec/newlib-1.18.0/newlib/libc/include/reent.h" 2



# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 150 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 3 4
typedef int ptrdiff_t;
# 213 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 3 4
typedef unsigned int size_t;
# 100 "/root/testvec/newlib-1.18.0/newlib/libc/include/reent.h" 2


struct stat;
struct tms;
struct timeval;
struct timezone;
# 140 "/root/testvec/newlib-1.18.0/newlib/libc/include/reent.h"
extern int _close_r (struct _reent *, int);
extern int _execve_r (struct _reent *, const char *, char *const *, char *const *);
extern int _fcntl_r (struct _reent *, int, int, int);
extern int _fork_r (struct _reent *);
extern int _fstat_r (struct _reent *, int, struct stat *);
extern int _getpid_r (struct _reent *);
extern int _isatty_r (struct _reent *, int);
extern int _kill_r (struct _reent *, int, int);
extern int _link_r (struct _reent *, const char *, const char *);
extern _off_t _lseek_r (struct _reent *, int, _off_t, int);
extern int _mkdir_r (struct _reent *, const char *, int);
extern int _open_r (struct _reent *, const char *, int, int);
extern _ssize_t _read_r (struct _reent *, int, void *, size_t);
extern int _rename_r (struct _reent *, const char *, const char *);
extern void *_sbrk_r (struct _reent *, ptrdiff_t);
extern int _stat_r (struct _reent *, const char *, struct stat *);
extern unsigned long _times_r (struct _reent *, struct tms *);
extern int _unlink_r (struct _reent *, const char *);
extern int _wait_r (struct _reent *, int *);
extern _ssize_t _write_r (struct _reent *, int, const void *, size_t);


extern int _gettimeofday_r (struct _reent *, struct timeval *__tp, void *__tzp);







struct stat64;

extern _off64_t _lseek64_r (struct _reent *, int, _off64_t, int);
extern int _fstat64_r (struct _reent *, int, struct stat64 *);
extern int _open64_r (struct _reent *, const char *, int, int);
extern int _stat64_r (struct _reent *, const char *, struct stat64 *);
# 153 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h" 1
# 29 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/_ansi.h" 1
# 30 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h" 2




# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 35 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h" 2


# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 38 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h" 2
# 46 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h" 1
# 54 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 55 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h" 2

# 1 "/usr/include/features.h" 1 3 4
# 323 "/usr/include/features.h" 3 4
# 1 "/usr/include/i386-linux-gnu/bits/predefs.h" 1 3 4
# 324 "/usr/include/features.h" 2 3 4
# 356 "/usr/include/features.h" 3 4
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/cdefs.h" 1 3 4
# 324 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/cdefs.h" 3 4
# 1 "/usr/include/i386-linux-gnu/bits/wordsize.h" 1 3 4
# 325 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/cdefs.h" 2 3 4
# 357 "/usr/include/features.h" 2 3 4
# 388 "/usr/include/features.h" 3 4
# 1 "/usr/include/i386-linux-gnu/gnu/stubs.h" 1 3 4



# 1 "/usr/include/i386-linux-gnu/bits/wordsize.h" 1 3 4
# 5 "/usr/include/i386-linux-gnu/gnu/stubs.h" 2 3 4


# 1 "/usr/include/i386-linux-gnu/gnu/stubs-32.h" 1 3 4
# 8 "/usr/include/i386-linux-gnu/gnu/stubs.h" 2 3 4
# 389 "/usr/include/features.h" 2 3 4
# 57 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h" 2






typedef long time_t;





typedef unsigned long clock_t;




typedef _ssize_t ssize_t;




__extension__ typedef long long quad_t;
__extension__ typedef unsigned long long u_quad_t;
# 91 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h"
typedef struct
  {
    int __val[2];
  } fsid_t;



typedef int clockid_t;
# 112 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h"
typedef long fd_mask;





typedef struct {
        unsigned long fds_bits [(1024/(8 * sizeof(unsigned long)))];
} __fd_set;
# 132 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h"
# 1 "/usr/include/linux/types.h" 1 3 4



# 1 "/usr/include/i386-linux-gnu/asm/types.h" 1 3 4



# 1 "/usr/include/asm-generic/types.h" 1 3 4






# 1 "/usr/include/asm-generic/int-ll64.h" 1 3 4
# 11 "/usr/include/asm-generic/int-ll64.h" 3 4
# 1 "/usr/include/i386-linux-gnu/asm/bitsperlong.h" 1 3 4
# 10 "/usr/include/i386-linux-gnu/asm/bitsperlong.h" 3 4
# 1 "/usr/include/asm-generic/bitsperlong.h" 1 3 4
# 11 "/usr/include/i386-linux-gnu/asm/bitsperlong.h" 2 3 4
# 12 "/usr/include/asm-generic/int-ll64.h" 2 3 4







typedef __signed__ char __s8;
typedef unsigned char __u8;

typedef __signed__ short __s16;
typedef unsigned short __u16;

typedef __signed__ int __s32;
typedef unsigned int __u32;


__extension__ typedef __signed__ long long __s64;
__extension__ typedef unsigned long long __u64;
# 8 "/usr/include/asm-generic/types.h" 2 3 4



typedef unsigned short umode_t;
# 5 "/usr/include/i386-linux-gnu/asm/types.h" 2 3 4
# 5 "/usr/include/linux/types.h" 2 3 4



# 1 "/usr/include/linux/posix_types.h" 1 3 4



# 1 "/usr/include/linux/stddef.h" 1 3 4
# 5 "/usr/include/linux/posix_types.h" 2 3 4
# 36 "/usr/include/linux/posix_types.h" 3 4
typedef struct {
 unsigned long fds_bits [(1024/(8 * sizeof(unsigned long)))];
} __kernel_fd_set;


typedef void (*__kernel_sighandler_t)(int);


typedef int __kernel_key_t;
typedef int __kernel_mqd_t;

# 1 "/usr/include/i386-linux-gnu/asm/posix_types.h" 1 3 4

# 1 "/usr/include/i386-linux-gnu/asm/posix_types_32.h" 1 3 4
# 10 "/usr/include/i386-linux-gnu/asm/posix_types_32.h" 3 4
typedef unsigned long __kernel_ino_t;
typedef unsigned short __kernel_mode_t;
typedef unsigned short __kernel_nlink_t;
typedef long __kernel_off_t;
typedef int __kernel_pid_t;
typedef unsigned short __kernel_ipc_pid_t;
typedef unsigned short __kernel_uid_t;
typedef unsigned short __kernel_gid_t;
typedef unsigned int __kernel_size_t;
typedef int __kernel_ssize_t;
typedef int __kernel_ptrdiff_t;
typedef long __kernel_time_t;
typedef long __kernel_suseconds_t;
typedef long __kernel_clock_t;
typedef int __kernel_timer_t;
typedef int __kernel_clockid_t;
typedef int __kernel_daddr_t;
typedef char * __kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;
typedef unsigned int __kernel_uid32_t;
typedef unsigned int __kernel_gid32_t;

typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;
typedef unsigned short __kernel_old_dev_t;


typedef long long __kernel_loff_t;


typedef struct {
 int val[2];
} __kernel_fsid_t;
# 3 "/usr/include/i386-linux-gnu/asm/posix_types.h" 2 3 4
# 48 "/usr/include/linux/posix_types.h" 2 3 4
# 9 "/usr/include/linux/types.h" 2 3 4
# 27 "/usr/include/linux/types.h" 3 4
typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;
typedef __u64 __le64;
typedef __u64 __be64;

typedef __u16 __sum16;
typedef __u32 __wsum;
# 133 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h" 2
# 1 "/usr/include/i386-linux-gnu/bits/types.h" 1 3 4
# 28 "/usr/include/i386-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/i386-linux-gnu/bits/wordsize.h" 1 3 4
# 29 "/usr/include/i386-linux-gnu/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;




__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;







__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
# 131 "/usr/include/i386-linux-gnu/bits/types.h" 3 4
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/typesizes.h" 1 3 4
# 132 "/usr/include/i386-linux-gnu/bits/types.h" 2 3 4


__extension__ typedef __u_quad_t __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef __u_quad_t __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef __quad_t __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct { int __val[2]; } __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned long int __rlim_t;
__extension__ typedef __u_quad_t __rlim64_t;
__extension__ typedef unsigned int __id_t;
__extension__ typedef long int __time_t;
__extension__ typedef unsigned int __useconds_t;
__extension__ typedef long int __suseconds_t;

__extension__ typedef int __daddr_t;
__extension__ typedef long int __swblk_t;
__extension__ typedef int __key_t;


__extension__ typedef int __clockid_t;


__extension__ typedef int __timer_t;


__extension__ typedef long int __blksize_t;




__extension__ typedef long int __blkcnt_t;
__extension__ typedef __quad_t __blkcnt64_t;


__extension__ typedef unsigned long int __fsblkcnt_t;
__extension__ typedef __u_quad_t __fsblkcnt64_t;


__extension__ typedef unsigned long int __fsfilcnt_t;
__extension__ typedef __u_quad_t __fsfilcnt64_t;

__extension__ typedef int __ssize_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


__extension__ typedef int __intptr_t;


__extension__ typedef unsigned int __socklen_t;
# 134 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h" 2
# 143 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h"
typedef __ino_t ino_t;
typedef __ino64_t ino64_t;
typedef __uint32_t uintptr_t;
typedef __int32_t intptr_t;
typedef __off64_t off64_t;
typedef __off_t off_t;
typedef __loff_t loff_t;
typedef __mode_t mode_t;
typedef __pid_t pid_t;
typedef __uid_t uid_t;
typedef __gid_t gid_t;
typedef __key_t key_t;
typedef __suseconds_t suseconds_t;
typedef __useconds_t useconds_t;
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
typedef __dev_t dev_t;
typedef __fd_set fd_set;
typedef __nlink_t nlink_t;

typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __uint8_t u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;
typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;


typedef __uint8_t uint8_t;




typedef __uint16_t uint16_t;




typedef __uint32_t uint32_t;




typedef __uint64_t uint64_t;



typedef __uint64_t u64;
# 208 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/types.h"
typedef unsigned int vm_offset_t;
typedef __int64_t vm_ooffset_t;
typedef unsigned int vm_pindex_t;
typedef unsigned int vm_size_t;

typedef __int32_t register_t;
typedef __uint32_t u_register_t;







typedef register_t critical_t;


typedef __uint32_t intrmask_t;


typedef void ointhand2_t(int _device_id);
# 47 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h" 2



typedef __FILE FILE;
# 59 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
typedef _fpos_t fpos_t;

typedef _fpos64_t fpos64_t;



# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/stdio.h" 1
# 19 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/stdio.h"
char * ctermid (char *);
# 66 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h" 2
# 175 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
FILE * tmpfile (void);
char * tmpnam (char *);
int fclose (FILE *);
int fflush (FILE *);
FILE * freopen (const char *, const char *, FILE *);
void setbuf (FILE *, char *);
int setvbuf (FILE *, char *, int, size_t);
int fprintf (FILE *, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
int fscanf (FILE *, const char *, ...) __attribute__ ((__format__ (__scanf__, 2, 3)))
                                                           ;
int printf (const char *, ...) __attribute__ ((__format__ (__printf__, 1, 2)))
                                                            ;
int scanf (const char *, ...) __attribute__ ((__format__ (__scanf__, 1, 2)))
                                                           ;
int sscanf (const char *, const char *, ...) __attribute__ ((__format__ (__scanf__, 2, 3)))
                                                           ;
int vfprintf (FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int vprintf (const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 1, 0)))
                                                            ;
int vsprintf (char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int fgetc (FILE *);
char * fgets (char *, int, FILE *);
int fputc (int, FILE *);
int fputs (const char *, FILE *);
int getc (FILE *);
int getchar (void);
char * gets (char *);
int putc (int, FILE *);
int putchar (int);
int puts (const char *);
int ungetc (int, FILE *);
size_t fread (void *, size_t _size, size_t _n, FILE *);
size_t fwrite (const void * , size_t _size, size_t _n, FILE *);



int fgetpos (FILE *, fpos_t *);

int fseek (FILE *, long, int);



int fsetpos (FILE *, const fpos_t *);

long ftell ( FILE *);
void rewind (FILE *);
void clearerr (FILE *);
int feof (FILE *);
int ferror (FILE *);
void perror (const char *);

FILE * fopen (const char *_name, const char *_type);
int sprintf (char *, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
int remove (const char *);
int rename (const char *, const char *);






int fseeko (FILE *, off_t, int);
off_t ftello ( FILE *);




int asiprintf (char **, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
char * asniprintf (char *, size_t *, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
char * asnprintf (char *, size_t *, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int asprintf (char **, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;

int diprintf (int, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;

int fcloseall (void);
int fiprintf (FILE *, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
int fiscanf (FILE *, const char *, ...) __attribute__ ((__format__ (__scanf__, 2, 3)))
                                                           ;
int iprintf (const char *, ...) __attribute__ ((__format__ (__printf__, 1, 2)))
                                                            ;
int iscanf (const char *, ...) __attribute__ ((__format__ (__scanf__, 1, 2)))
                                                           ;
int siprintf (char *, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
int siscanf (const char *, const char *, ...) __attribute__ ((__format__ (__scanf__, 2, 3)))
                                                           ;
int snprintf (char *, size_t, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int sniprintf (char *, size_t, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
char * tempnam (const char *, const char *);
int vasiprintf (char **, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
char * vasniprintf (char *, size_t *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
char * vasnprintf (char *, size_t *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int vasprintf (char **, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int vdiprintf (int, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int vfiprintf (FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int vfiscanf (FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 2, 0)))
                                                           ;
int vfscanf (FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 2, 0)))
                                                           ;
int viprintf (const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 1, 0)))
                                                            ;
int viscanf (const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 1, 0)))
                                                           ;
int vscanf (const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 1, 0)))
                                                           ;
int vsiprintf (char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int vsiscanf (const char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 2, 0)))
                                                           ;
int vsniprintf (char *, size_t, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int vsnprintf (char *, size_t, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int vsscanf (const char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 2, 0)))
                                                           ;
# 317 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
FILE * fdopen (int, const char *);

int fileno (FILE *);
int getw (FILE *);
int pclose (FILE *);
FILE * popen (const char *, const char *);
int putw (int, FILE *);
void setbuffer (FILE *, char *, int);
int setlinebuf (FILE *);
int getc_unlocked (FILE *);
int getchar_unlocked (void);
void flockfile (FILE *);
int ftrylockfile (FILE *);
void funlockfile (FILE *);
int putc_unlocked (int, FILE *);
int putchar_unlocked (int);
# 342 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
int dprintf (int, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;

FILE * fmemopen (void *, size_t, const char *);


FILE * open_memstream (char **, size_t *);



int vdprintf (int, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;







int _asiprintf_r (struct _reent *, char **, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
char * _asniprintf_r (struct _reent *, char *, size_t *, const char *, ...) __attribute__ ((__format__ (__printf__, 4, 5)))
                                                            ;
char * _asnprintf_r (struct _reent *, char *, size_t *, const char *, ...) __attribute__ ((__format__ (__printf__, 4, 5)))
                                                            ;
int _asprintf_r (struct _reent *, char **, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _diprintf_r (struct _reent *, int, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _dprintf_r (struct _reent *, int, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _fclose_r (struct _reent *, FILE *);
int _fcloseall_r (struct _reent *);
FILE * _fdopen_r (struct _reent *, int, const char *);
int _fflush_r (struct _reent *, FILE *);
int _fgetc_r (struct _reent *, FILE *);
char * _fgets_r (struct _reent *, char *, int, FILE *);




int _fgetpos_r (struct _reent *, FILE *, fpos_t *);
int _fsetpos_r (struct _reent *, FILE *, const fpos_t *);

int _fiprintf_r (struct _reent *, FILE *, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _fiscanf_r (struct _reent *, FILE *, const char *, ...) __attribute__ ((__format__ (__scanf__, 3, 4)))
                                                           ;
FILE * _fmemopen_r (struct _reent *, void *, size_t, const char *);
FILE * _fopen_r (struct _reent *, const char *, const char *);
FILE * _freopen_r (struct _reent *, const char *, const char *, FILE *);
int _fprintf_r (struct _reent *, FILE *, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _fpurge_r (struct _reent *, FILE *);
int _fputc_r (struct _reent *, int, FILE *);
int _fputs_r (struct _reent *, const char *, FILE *);
size_t _fread_r (struct _reent *, void *, size_t _size, size_t _n, FILE *);
int _fscanf_r (struct _reent *, FILE *, const char *, ...) __attribute__ ((__format__ (__scanf__, 3, 4)))
                                                           ;
int _fseek_r (struct _reent *, FILE *, long, int);
int _fseeko_r (struct _reent *, FILE *, _off_t, int);
long _ftell_r (struct _reent *, FILE *);
_off_t _ftello_r (struct _reent *, FILE *);
void _rewind_r (struct _reent *, FILE *);
size_t _fwrite_r (struct _reent *, const void * , size_t _size, size_t _n, FILE *);
int _getc_r (struct _reent *, FILE *);
int _getc_unlocked_r (struct _reent *, FILE *);
int _getchar_r (struct _reent *);
int _getchar_unlocked_r (struct _reent *);
char * _gets_r (struct _reent *, char *);
int _iprintf_r (struct _reent *, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
int _iscanf_r (struct _reent *, const char *, ...) __attribute__ ((__format__ (__scanf__, 2, 3)))
                                                           ;
FILE * _open_memstream_r (struct _reent *, char **, size_t *);
void _perror_r (struct _reent *, const char *);
int _printf_r (struct _reent *, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)))
                                                            ;
int _putc_r (struct _reent *, int, FILE *);
int _putc_unlocked_r (struct _reent *, int, FILE *);
int _putchar_unlocked_r (struct _reent *, int);
int _putchar_r (struct _reent *, int);
int _puts_r (struct _reent *, const char *);
int _remove_r (struct _reent *, const char *);
int _rename_r (struct _reent *, const char *_old, const char *_new)
                                          ;
int _scanf_r (struct _reent *, const char *, ...) __attribute__ ((__format__ (__scanf__, 2, 3)))
                                                           ;
int _siprintf_r (struct _reent *, char *, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _siscanf_r (struct _reent *, const char *, const char *, ...) __attribute__ ((__format__ (__scanf__, 3, 4)))
                                                           ;
int _sniprintf_r (struct _reent *, char *, size_t, const char *, ...) __attribute__ ((__format__ (__printf__, 4, 5)))
                                                            ;
int _snprintf_r (struct _reent *, char *, size_t, const char *, ...) __attribute__ ((__format__ (__printf__, 4, 5)))
                                                            ;
int _sprintf_r (struct _reent *, char *, const char *, ...) __attribute__ ((__format__ (__printf__, 3, 4)))
                                                            ;
int _sscanf_r (struct _reent *, const char *, const char *, ...) __attribute__ ((__format__ (__scanf__, 3, 4)))
                                                           ;
char * _tempnam_r (struct _reent *, const char *, const char *);
FILE * _tmpfile_r (struct _reent *);
char * _tmpnam_r (struct _reent *, char *);
int _ungetc_r (struct _reent *, int, FILE *);
int _vasiprintf_r (struct _reent *, char **, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
char * _vasniprintf_r (struct _reent*, char *, size_t *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 4, 0)))
                                                            ;
char * _vasnprintf_r (struct _reent*, char *, size_t *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 4, 0)))
                                                            ;
int _vasprintf_r (struct _reent *, char **, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vdiprintf_r (struct _reent *, int, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vdprintf_r (struct _reent *, int, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vfiprintf_r (struct _reent *, FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vfiscanf_r (struct _reent *, FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 3, 0)))
                                                           ;
int _vfprintf_r (struct _reent *, FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vfscanf_r (struct _reent *, FILE *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 3, 0)))
                                                           ;
int _viprintf_r (struct _reent *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int _viscanf_r (struct _reent *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 2, 0)))
                                                           ;
int _vprintf_r (struct _reent *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 2, 0)))
                                                            ;
int _vscanf_r (struct _reent *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 2, 0)))
                                                           ;
int _vsiprintf_r (struct _reent *, char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vsiscanf_r (struct _reent *, const char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 3, 0)))
                                                           ;
int _vsniprintf_r (struct _reent *, char *, size_t, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 4, 0)))
                                                            ;
int _vsnprintf_r (struct _reent *, char *, size_t, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 4, 0)))
                                                            ;
int _vsprintf_r (struct _reent *, char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__printf__, 3, 0)))
                                                            ;
int _vsscanf_r (struct _reent *, const char *, const char *, __gnuc_va_list) __attribute__ ((__format__ (__scanf__, 3, 0)))
                                                           ;



int fpurge (FILE *);
ssize_t __getdelim (char **, size_t *, int, FILE *);
ssize_t __getline (char **, size_t *, FILE *);



FILE * fdopen64 (int, const char *);
FILE * fopen64 (const char *, const char *);
FILE * freopen64 (const char *, const char *, FILE *);
_off64_t ftello64 (FILE *);
_off64_t fseeko64 (FILE *, _off64_t, int);
int fgetpos64 (FILE *, _fpos64_t *);
int fsetpos64 (FILE *, const _fpos64_t *);
FILE * tmpfile64 (void);

FILE * _fdopen64_r (struct _reent *, int, const char *);
FILE * _fopen64_r (struct _reent *,const char *, const char *);
FILE * _freopen64_r (struct _reent *, const char *, const char *, FILE *);
_off64_t _ftello64_r (struct _reent *, FILE *);
_off64_t _fseeko64_r (struct _reent *, FILE *, _off64_t, int);
int _fgetpos64_r (struct _reent *, FILE *, _fpos64_t *);
int _fsetpos64_r (struct _reent *, FILE *, const _fpos64_t *);
FILE * _tmpfile64_r (struct _reent *);







int __srget_r (struct _reent *, FILE *);
int __swbuf_r (struct _reent *, int, FILE *);







FILE *funopen (const void * __cookie, int (*__readfn)(void * __c, char *__buf, int __n), int (*__writefn)(void * __c, const char *__buf, int __n), _fpos64_t (*__seekfn)(void * __c, _fpos64_t __off, int __whence), int (*__closefn)(void * __c))



                              ;
FILE *_funopen_r (struct _reent *, const void * __cookie, int (*__readfn)(void * __c, char *__buf, int __n), int (*__writefn)(void * __c, const char *__buf, int __n), _fpos64_t (*__seekfn)(void * __c, _fpos64_t __off, int __whence), int (*__closefn)(void * __c))



                              ;
# 556 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"
typedef ssize_t cookie_read_function_t(void *__cookie, char *__buf, size_t __n);
typedef ssize_t cookie_write_function_t(void *__cookie, const char *__buf,
     size_t __n);

typedef int cookie_seek_function_t(void *__cookie, _off64_t *__off,
       int __whence);



typedef int cookie_close_function_t(void *__cookie);
typedef struct
{


  cookie_read_function_t *read;
  cookie_write_function_t *write;
  cookie_seek_function_t *seek;
  cookie_close_function_t *close;
} cookie_io_functions_t;
FILE *fopencookie (void *__cookie, const char *__mode, cookie_io_functions_t __functions)
                                                         ;
FILE *_fopencookie_r (struct _reent *, void *__cookie, const char *__mode, cookie_io_functions_t __functions)
                                                         ;
# 687 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdio.h"

# 154 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h" 1
# 10 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/ieeefp.h" 1
# 11 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h" 2




# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 325 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 3 4
typedef int wchar_t;
# 16 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h" 2


# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/stdlib.h" 1
# 19 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h" 2

# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/alloca.h" 1
# 21 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h" 2








typedef struct
{
  int quot;
  int rem;
} div_t;

typedef struct
{
  long quot;
  long rem;
} ldiv_t;


typedef struct
{
  long long int quot;
  long long int rem;
} lldiv_t;
# 58 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h"
extern int __mb_cur_max;



void abort (void) __attribute__ ((noreturn));
int abs (int);
int atexit (void (*__func)(void));
double atof (const char *__nptr);

float atoff (const char *__nptr);

int atoi (const char *__nptr);
int _atoi_r (struct _reent *, const char *__nptr);
long atol (const char *__nptr);
long _atol_r (struct _reent *, const char *__nptr);
void * bsearch (const void * __key, const void * __base, size_t __nmemb, size_t __size, int (* _compar) (const void *, const void *))



                                                         ;
void * calloc (size_t __nmemb, size_t __size) ;
div_t div (int __numer, int __denom);
void exit (int __status) __attribute__ ((noreturn));
void free (void *) ;
char * getenv (const char *__string);
char * _getenv_r (struct _reent *, const char *__string);
char * _findenv (const char *, int *);
char * _findenv_r (struct _reent *, const char *, int *);
long labs (long);
ldiv_t ldiv (long __numer, long __denom);
void * malloc (size_t __size) ;
int mblen (const char *, size_t);
int _mblen_r (struct _reent *, const char *, size_t, _mbstate_t *);
int mbtowc (wchar_t *, const char *, size_t);
int _mbtowc_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
int wctomb (char *, wchar_t);
int _wctomb_r (struct _reent *, char *, wchar_t, _mbstate_t *);
size_t mbstowcs (wchar_t *, const char *, size_t);
size_t _mbstowcs_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
size_t wcstombs (char *, const wchar_t *, size_t);
size_t _wcstombs_r (struct _reent *, char *, const wchar_t *, size_t, _mbstate_t *);


char * mkdtemp (char *);
int mkstemp (char *);
int mkstemps (char *, int);
char * mktemp (char *) __attribute__ ((warning ("the use of `mktemp' is dangerous; use `mkstemp' instead")));

char * _mkdtemp_r (struct _reent *, char *);
int _mkstemp_r (struct _reent *, char *);
int _mkstemps_r (struct _reent *, char *, int);
char * _mktemp_r (struct _reent *, char *) __attribute__ ((warning ("the use of `mktemp' is dangerous; use `mkstemp' instead")));

void qsort (void * __base, size_t __nmemb, size_t __size, int(*_compar)(const void *, const void *));
int rand (void);
void * realloc (void * __r, size_t __size) ;

void * reallocf (void * __r, size_t __size);

void srand (unsigned __seed);
double strtod (const char *__n, char **__end_PTR);
double _strtod_r (struct _reent *,const char *__n, char **__end_PTR);
float strtof (const char *__n, char **__end_PTR);






long strtol (const char *__n, char **__end_PTR, int __base);
long _strtol_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
unsigned long strtoul (const char *__n, char **__end_PTR, int __base);
unsigned long _strtoul_r (struct _reent *,const char *__n, char **__end_PTR, int __base);

int system (const char *__string);


long a64l (const char *__input);
char * l64a (long __input);
char * _l64a_r (struct _reent *,long __input);
int on_exit (void (*__func)(int, void *),void * __arg);
void _Exit (int __status) __attribute__ ((noreturn));
int putenv (char *__string);
int _putenv_r (struct _reent *, char *__string);
void * _reallocf_r (struct _reent *, void *, size_t);
int setenv (const char *__string, const char *__value, int __overwrite);
int _setenv_r (struct _reent *, const char *__string, const char *__value, int __overwrite);

char * gcvt (double,int,char *);
char * gcvtf (float,int,char *);
char * fcvt (double,int,int *,int *);
char * fcvtf (float,int,int *,int *);
char * ecvt (double,int,int *,int *);
char * ecvtbuf (double, int, int*, int*, char *);
char * fcvtbuf (double, int, int*, int*, char *);
char * ecvtf (float,int,int *,int *);
char * dtoa (double, int, int, int *, int*, char**);
int rand_r (unsigned *__seed);

double drand48 (void);
double _drand48_r (struct _reent *);
double erand48 (unsigned short [3]);
double _erand48_r (struct _reent *, unsigned short [3]);
long jrand48 (unsigned short [3]);
long _jrand48_r (struct _reent *, unsigned short [3]);
void lcong48 (unsigned short [7]);
void _lcong48_r (struct _reent *, unsigned short [7]);
long lrand48 (void);
long _lrand48_r (struct _reent *);
long mrand48 (void);
long _mrand48_r (struct _reent *);
long nrand48 (unsigned short [3]);
long _nrand48_r (struct _reent *, unsigned short [3]);
unsigned short *
       seed48 (unsigned short [3]);
unsigned short *
       _seed48_r (struct _reent *, unsigned short [3]);
void srand48 (long);
void _srand48_r (struct _reent *, long);
long long atoll (const char *__nptr);
long long _atoll_r (struct _reent *, const char *__nptr);
long long llabs (long long);
lldiv_t lldiv (long long __numer, long long __denom);
long long strtoll (const char *__n, char **__end_PTR, int __base);
long long _strtoll_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
unsigned long long strtoull (const char *__n, char **__end_PTR, int __base);
unsigned long long _strtoull_r (struct _reent *, const char *__n, char **__end_PTR, int __base);


void cfree (void *);
int unsetenv (const char *__string);
int _unsetenv_r (struct _reent *, const char *__string);
# 198 "/root/testvec/newlib-1.18.0/newlib/libc/include/stdlib.h"
char * _dtoa_r (struct _reent *, double, int, int, int *, int*, char**);

void * _malloc_r (struct _reent *, size_t) ;
void * _calloc_r (struct _reent *, size_t, size_t) ;
void _free_r (struct _reent *, void *) ;
void * _realloc_r (struct _reent *, void *, size_t) ;
void _mstats_r (struct _reent *, char *);

int _system_r (struct _reent *, const char *);

void __eprintf (const char *, const char *, unsigned int, const char *);








# 155 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/string.h" 1
# 14 "/root/testvec/newlib-1.18.0/newlib/libc/include/string.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 15 "/root/testvec/newlib-1.18.0/newlib/libc/include/string.h" 2







void * memchr (const void *, int, size_t);
int memcmp (const void *, const void *, size_t);
void * memcpy (void *, const void *, size_t);
void * memmove (void *, const void *, size_t);
void * memset (void *, int, size_t);
char *strcat (char *, const char *);
char *strchr (const char *, int);
int strcmp (const char *, const char *);
int strcoll (const char *, const char *);
char *strcpy (char *, const char *);
size_t strcspn (const char *, const char *);
char *strerror (int);
size_t strlen (const char *);
char *strncat (char *, const char *, size_t);
int strncmp (const char *, const char *, size_t);
char *strncpy (char *, const char *, size_t);
char *strpbrk (const char *, const char *);
char *strrchr (const char *, int);
size_t strspn (const char *, const char *);
char *strstr (const char *, const char *);


char *strtok (char *, const char *);


size_t strxfrm (char *, const char *, size_t);


char *strtok_r (char *, const char *, char **);

int bcmp (const void *, const void *, size_t);
void bcopy (const void *, void *, size_t);
void bzero (void *, size_t);
int ffs (int);
char *index (const char *, int);
void * memccpy (void *, const void *, int, size_t);
void * mempcpy (void *, const void *, size_t);
void * memmem (const void *, size_t, const void *, size_t);
char *rindex (const char *, int);
char *stpcpy (char *, const char *);
char *stpncpy (char *, const char *, size_t);
int strcasecmp (const char *, const char *);
char *strcasestr (const char *, const char *);
char *strdup (const char *);
char *_strdup_r (struct _reent *, const char *);
char *strndup (const char *, size_t);
char *_strndup_r (struct _reent *, const char *, size_t);
char *strerror_r (int, char *, size_t);
size_t strlcat (char *, const char *, size_t);
size_t strlcpy (char *, const char *, size_t);
int strncasecmp (const char *, const char *, size_t);
size_t strnlen (const char *, size_t);
char *strsep (char **, const char *);
char *strlwr (char *);
char *strupr (char *);
# 100 "/root/testvec/newlib-1.18.0/newlib/libc/include/string.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/string.h" 1




char *strsignal (int __signo);
# 101 "/root/testvec/newlib-1.18.0/newlib/libc/include/string.h" 2


# 156 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h" 1
# 124 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/limits.h" 1
# 130 "/root/testvec/newlib-1.18.0/newlib/libc/include/limits.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include-fixed/limits.h" 1 3 4
# 131 "/root/testvec/newlib-1.18.0/newlib/libc/include/limits.h" 2
# 125 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h" 2
# 142 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h"
# 1 "/usr/include/linux/limits.h" 1 3 4
# 143 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h" 2



# 1 "/usr/include/i386-linux-gnu/bits/posix1_lim.h" 1 3 4
# 157 "/usr/include/i386-linux-gnu/bits/posix1_lim.h" 3 4
# 1 "/usr/include/i386-linux-gnu/bits/local_lim.h" 1 3 4
# 158 "/usr/include/i386-linux-gnu/bits/posix1_lim.h" 2 3 4
# 147 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h" 2



# 1 "/usr/include/i386-linux-gnu/bits/posix2_lim.h" 1 3 4
# 151 "/root/testvec/newlib-1.18.0/newlib/targ-include/limits.h" 2
# 157 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h" 1
# 18 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
# 1 "/usr/include/i386-linux-gnu/bits/wordsize.h" 1 3 4
# 19 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h" 2
# 37 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
typedef signed char int_least8_t;
typedef unsigned char uint_least8_t;
# 51 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
typedef int16_t int_least16_t;
typedef uint16_t uint_least16_t;
# 74 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
typedef int32_t int_least32_t;
typedef uint32_t uint_least32_t;
# 94 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
typedef signed char int_fast8_t;





typedef int int_fast16_t;
typedef int int_fast32_t;
__extension__
typedef long long int int_fast64_t;



typedef unsigned char uint_fast8_t;





typedef unsigned int uint_fast16_t;
typedef unsigned int uint_fast32_t;
__extension__
typedef unsigned long long int uint_fast64_t;
# 133 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
typedef int64_t int_least64_t;
typedef uint64_t uint_least64_t;
# 159 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
  typedef long long int intmax_t;
# 168 "/root/testvec/newlib-1.18.0/newlib/targ-include/stdint.h"
  typedef long long unsigned int uintmax_t;
# 158 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h" 1
# 11 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 12 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h" 2


# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stdarg.h" 1 3 4
# 15 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h" 2
# 39 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h"




struct tm;



typedef _mbstate_t mbstate_t;


wint_t btowc (int);
int wctob (wint_t);
size_t mbrlen (const char * , size_t, mbstate_t *);
size_t mbrtowc (wchar_t * , const char * , size_t, mbstate_t *);
size_t _mbrtowc_r (struct _reent *, wchar_t * , const char * , size_t, mbstate_t *)
                        ;
int mbsinit (const mbstate_t *);
size_t mbsnrtowcs (wchar_t * , const char ** , size_t, size_t, mbstate_t *)
                ;
size_t _mbsnrtowcs_r (struct _reent *, wchar_t * , const char ** , size_t, size_t, mbstate_t *)
                                ;
size_t mbsrtowcs (wchar_t * , const char ** , size_t, mbstate_t *);
size_t _mbsrtowcs_r (struct _reent *, wchar_t * , const char ** , size_t, mbstate_t *);
size_t wcrtomb (char * , wchar_t, mbstate_t *);
size_t _wcrtomb_r (struct _reent *, char * , wchar_t, mbstate_t *);
size_t wcsnrtombs (char * , const wchar_t ** , size_t, size_t, mbstate_t *)
                ;
size_t _wcsnrtombs_r (struct _reent *, char * , const wchar_t ** , size_t, size_t, mbstate_t *)
                                ;
size_t wcsrtombs (char * , const wchar_t ** , size_t, mbstate_t *);
size_t _wcsrtombs_r (struct _reent *, char * , const wchar_t ** , size_t, mbstate_t *)
                        ;
int wcscasecmp (const wchar_t *, const wchar_t *);
wchar_t *wcscat (wchar_t * , const wchar_t *);
wchar_t *wcschr (const wchar_t *, wchar_t);
int wcscmp (const wchar_t *, const wchar_t *);
int wcscoll (const wchar_t *, const wchar_t *);
wchar_t *wcscpy (wchar_t * , const wchar_t *);
wchar_t *wcpcpy (wchar_t * , const wchar_t *);
wchar_t *wcsdup (const wchar_t *);
wchar_t *_wcsdup_r (struct _reent *, const wchar_t * );
size_t wcscspn (const wchar_t *, const wchar_t *);
size_t wcsftime (wchar_t *, size_t, const wchar_t *, const struct tm *);
size_t wcslcat (wchar_t *, const wchar_t *, size_t);
size_t wcslcpy (wchar_t *, const wchar_t *, size_t);
size_t wcslen (const wchar_t *);
int wcsncasecmp (const wchar_t *, const wchar_t *, size_t);
wchar_t *wcsncat (wchar_t * , const wchar_t * , size_t);
int wcsncmp (const wchar_t *, const wchar_t *, size_t);
wchar_t *wcsncpy (wchar_t * , const wchar_t * , size_t);
wchar_t *wcpncpy (wchar_t * , const wchar_t * , size_t);
size_t wcsnlen (const wchar_t *, size_t);
wchar_t *wcspbrk (const wchar_t *, const wchar_t *);
wchar_t *wcsrchr (const wchar_t *, wchar_t);
size_t wcsspn (const wchar_t *, const wchar_t *);
wchar_t *wcsstr (const wchar_t *, const wchar_t *);
wchar_t *wcstok (wchar_t *, const wchar_t *, wchar_t **);
double wcstod (const wchar_t *, wchar_t **);
double _wcstod_r (struct _reent *, const wchar_t *, wchar_t **);
float wcstof (const wchar_t *, wchar_t **);
float _wcstof_r (struct _reent *, const wchar_t *, wchar_t **);
int wcswidth (const wchar_t *, size_t);
size_t wcsxfrm (wchar_t *, const wchar_t *, size_t);
int wcwidth (const wchar_t);
wchar_t *wmemchr (const wchar_t *, wchar_t, size_t);
int wmemcmp (const wchar_t *, const wchar_t *, size_t);
wchar_t *wmemcpy (wchar_t * , const wchar_t * , size_t);
wchar_t *wmemmove (wchar_t *, const wchar_t *, size_t);
wchar_t *wmemset (wchar_t *, wchar_t, size_t);

long wcstol (const wchar_t *, wchar_t **, int);
long long wcstoll (const wchar_t *, wchar_t **, int);
unsigned long wcstoul (const wchar_t *, wchar_t **, int);
unsigned long long wcstoull (const wchar_t *, wchar_t **, int);
long _wcstol_r (struct _reent *, const wchar_t *, wchar_t **, int);
long long _wcstoll_r (struct _reent *, const wchar_t *, wchar_t **, int);
unsigned long _wcstoul_r (struct _reent *, const wchar_t *, wchar_t **, int);
unsigned long long _wcstoull_r (struct _reent *, const wchar_t *, wchar_t **, int);

wint_t fgetwc (__FILE *);
wchar_t *fgetws (wchar_t *, int, __FILE *);
wint_t fputwc (wchar_t, __FILE *);
int fputws (const wchar_t *, __FILE *);
int fwide (__FILE *, int);
wint_t getwc (__FILE *);
wint_t getwchar (void);
wint_t putwc (wchar_t, __FILE *);
wint_t putwchar (wchar_t);
wint_t ungetwc (wint_t wc, __FILE *);

wint_t _fgetwc_r (struct _reent *, __FILE *);
wchar_t *_fgetws_r (struct _reent *, wchar_t *, int, __FILE *);
wint_t _fputwc_r (struct _reent *, wchar_t, __FILE *);
int _fputws_r (struct _reent *, const wchar_t *, __FILE *);
int _fwide_r (struct _reent *, __FILE *, int);
wint_t _getwc_r (struct _reent *, __FILE *);
wint_t _getwchar_r (struct _reent *ptr);
wint_t _putwc_r (struct _reent *, wchar_t, __FILE *);
wint_t _putwchar_r (struct _reent *, wchar_t);
wint_t _ungetwc_r (struct _reent *, wint_t wc, __FILE *);

__FILE *open_wmemstream (wchar_t **, size_t *);
__FILE *_open_wmemstream_r (struct _reent *, wchar_t **, size_t *);
# 152 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h"
int fwprintf (__FILE *, const wchar_t *, ...);
int swprintf (wchar_t *, size_t, const wchar_t *, ...);
int vfwprintf (__FILE *, const wchar_t *, __gnuc_va_list);
int vswprintf (wchar_t *, size_t, const wchar_t *, __gnuc_va_list);
int vwprintf (const wchar_t *, __gnuc_va_list);
int wprintf (const wchar_t *, ...);

int _fwprintf_r (struct _reent *, __FILE *, const wchar_t *, ...);
int _swprintf_r (struct _reent *, wchar_t *, size_t, const wchar_t *, ...);
int _vfwprintf_r (struct _reent *, __FILE *, const wchar_t *, __gnuc_va_list);
int _vswprintf_r (struct _reent *, wchar_t *, size_t, const wchar_t *, __gnuc_va_list);
int _vwprintf_r (struct _reent *, const wchar_t *, __gnuc_va_list);
int _wprintf_r (struct _reent *, const wchar_t *, ...);

int fwscanf (__FILE *, const wchar_t *, ...);
int swscanf (const wchar_t *, const wchar_t *, ...);
int vfwscanf (__FILE *, const wchar_t *, __gnuc_va_list);
int vswscanf (const wchar_t *, const wchar_t *, __gnuc_va_list);
int vwscanf (const wchar_t *, __gnuc_va_list);
int wscanf (const wchar_t *, ...);

int _fwscanf_r (struct _reent *, __FILE *, const wchar_t *, ...);
int _swscanf_r (struct _reent *, const wchar_t *, const wchar_t *, ...);
int _vfwscanf_r (struct _reent *, __FILE *, const wchar_t *, __gnuc_va_list);
int _vswscanf_r (struct _reent *, const wchar_t *, const wchar_t *, __gnuc_va_list);
int _vwscanf_r (struct _reent *, const wchar_t *, __gnuc_va_list);
int _wscanf_r (struct _reent *, const wchar_t *, ...);
# 190 "/root/testvec/newlib-1.18.0/newlib/libc/include/wchar.h"

# 159 "./vfprintf.c" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/lock.h" 1
# 13 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/lock.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/libc-lock.h" 1
# 24 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/libc-lock.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h" 1
# 20 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sched.h" 1
# 26 "/root/testvec/newlib-1.18.0/newlib/targ-include/sched.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 27 "/root/testvec/newlib-1.18.0/newlib/targ-include/sched.h" 2





# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 1
# 11 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/linux_time.h" 1



# 1 "/usr/include/i386-linux-gnu/asm/param.h" 1 3 4
# 1 "/usr/include/asm-generic/param.h" 1 3 4
# 1 "/usr/include/i386-linux-gnu/asm/param.h" 2 3 4
# 5 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/linux_time.h" 2




struct timespec {
 time_t tv_sec;
 long tv_nsec;
};
# 29 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/linux_time.h"
static __inline__ unsigned long
timespec_to_jiffies(struct timespec *value)
{
 unsigned long sec = value->tv_sec;
 long nsec = value->tv_nsec;

 if (sec >= (((~0UL >> 1)-1) / 100))
  return ((~0UL >> 1)-1);
 nsec += 1000000000L / 100 - 1;
 nsec /= 1000000000L / 100;
 return 100 * sec + nsec;
}

static __inline__ void
jiffies_to_timespec(unsigned long jiffies, struct timespec *value)
{
 value->tv_nsec = (jiffies % 100) * (1000000000L / 100);
 value->tv_sec = jiffies / 100;
}



struct timeval {
 time_t tv_sec;
 suseconds_t tv_usec;
};


struct timezone {
 int tz_minuteswest;
 int tz_dsttime;
};





struct itimerspec {
        struct timespec it_interval;
        struct timespec it_value;
};

struct itimerval {
 struct timeval it_interval;
 struct timeval it_value;
};
# 12 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 2
# 22 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/time.h" 1
# 23 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 2
# 33 "/root/testvec/newlib-1.18.0/newlib/targ-include/sched.h" 2


# 1 "/usr/include/i386-linux-gnu/bits/sched.h" 1 3 4
# 74 "/usr/include/i386-linux-gnu/bits/sched.h" 3 4
struct sched_param
  {
    int __sched_priority;
  };





extern int clone (int (*__fn) (void *__arg), void *__child_stack,
    int __flags, void *__arg, ...) __attribute__ ((__nothrow__));


extern int unshare (int __flags) __attribute__ ((__nothrow__));


extern int sched_getcpu (void) __attribute__ ((__nothrow__));










struct __sched_param
  {
    int __sched_priority;
  };
# 116 "/usr/include/i386-linux-gnu/bits/sched.h" 3 4
typedef unsigned long int __cpu_mask;






typedef struct
{
  __cpu_mask __bits[1024 / (8 * sizeof (__cpu_mask))];
} cpu_set_t;
# 199 "/usr/include/i386-linux-gnu/bits/sched.h" 3 4


extern int __sched_cpucount (size_t __setsize, const cpu_set_t *__setp)
  __attribute__ ((__nothrow__));
extern cpu_set_t *__sched_cpualloc (size_t __count) __attribute__ ((__nothrow__)) ;
extern void __sched_cpufree (cpu_set_t *__set) __attribute__ ((__nothrow__));


# 36 "/root/testvec/newlib-1.18.0/newlib/targ-include/sched.h" 2







extern int sched_setparam (__pid_t __pid, __const struct sched_param *__param)
     __attribute__ ((__nothrow__));


extern int sched_getparam (__pid_t __pid, struct sched_param *__param) __attribute__ ((__nothrow__));


extern int sched_setscheduler (__pid_t __pid, int __policy,
          __const struct sched_param *__param) __attribute__ ((__nothrow__));


extern int sched_getscheduler (__pid_t __pid) __attribute__ ((__nothrow__));


extern int sched_yield (void) __attribute__ ((__nothrow__));


extern int sched_get_priority_max (int __algorithm) __attribute__ ((__nothrow__));


extern int sched_get_priority_min (int __algorithm) __attribute__ ((__nothrow__));


extern int sched_rr_get_interval (__pid_t __pid, struct timespec *__t) __attribute__ ((__nothrow__));
# 80 "/root/testvec/newlib-1.18.0/newlib/targ-include/sched.h"
extern int sched_setaffinity (__pid_t __pid, size_t __cpusetsize,
         __const cpu_set_t *__cpuset) __attribute__ ((__nothrow__));


extern int sched_getaffinity (__pid_t __pid, size_t __cpusetsize,
         cpu_set_t *__cpuset) __attribute__ ((__nothrow__));



# 21 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 1
# 45 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 46 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 2

struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
};



typedef __timer_t timer_t;


clock_t clock (void);
double difftime (time_t _time2, time_t _time1);
time_t mktime (struct tm *_timeptr);
time_t time (time_t *_timer);

char *asctime (const struct tm *_tblock);
char *ctime (const time_t *_time);
struct tm *gmtime (const time_t *_timer);
struct tm *localtime (const time_t *_timer);

size_t strftime (char *_s, size_t _maxsize, const char *_fmt, const struct tm *_t);

char *asctime_r (const struct tm *, char *);
char *ctime_r (const time_t *, char *);
struct tm *gmtime_r (const time_t *, struct tm *);
struct tm *localtime_r (const time_t *, struct tm *);


char *strptime (const char *, const char *, struct tm *);
void tzset (void);
void _tzset_r (struct _reent *);

typedef struct __tzrule_struct
{
  char ch;
  int m;
  int n;
  int d;
  int s;
  time_t change;
  long offset;
} __tzrule_type;

typedef struct __tzinfo_struct
{
  int __tznorth;
  int __tzyear;
  __tzrule_type __tzrule[2];
} __tzinfo_type;

__tzinfo_type *__gettzinfo (void);





int *__getdate_err (void);

struct tm * getdate (const char *);
# 126 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h"
int getdate_r (const char *, struct tm *);


extern long _timezone;
extern int _daylight;
extern char *_tzname[2];
# 140 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/features.h" 1



# 1 "/usr/include/i386-linux-gnu/bits/posix_opt.h" 1 3 4
# 5 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/features.h" 2
# 141 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 2



# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/signal.h" 1




# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h" 1
# 11 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h"
# 1 "/usr/include/i386-linux-gnu/bits/sigset.h" 1 3 4
# 24 "/usr/include/i386-linux-gnu/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 104 "/usr/include/i386-linux-gnu/bits/sigset.h" 3 4
extern int __sigismember (__const __sigset_t *, int);
extern int __sigaddset (__sigset_t *, int);
extern int __sigdelset (__sigset_t *, int);
# 118 "/usr/include/i386-linux-gnu/bits/sigset.h" 3 4
extern __inline int __sigismember (__const __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return (__set->__val[__word] & __mask) ? 1 : 0; }
extern __inline int __sigaddset ( __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return ((__set->__val[__word] |= __mask), 0); }
extern __inline int __sigdelset ( __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return ((__set->__val[__word] &= ~__mask), 0); }
# 12 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h" 2
# 1 "/usr/include/i386-linux-gnu/bits/signum.h" 1 3 4
# 13 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h" 2
# 22 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h"
typedef void (*_sig_func_ptr) (int);
typedef _sig_func_ptr __sighandler_t;

# 1 "/usr/include/i386-linux-gnu/bits/siginfo.h" 1 3 4
# 25 "/usr/include/i386-linux-gnu/bits/siginfo.h" 3 4
# 1 "/usr/include/i386-linux-gnu/bits/wordsize.h" 1 3 4
# 26 "/usr/include/i386-linux-gnu/bits/siginfo.h" 2 3 4







typedef union sigval
  {
    int sival_int;
    void *sival_ptr;
  } sigval_t;
# 51 "/usr/include/i386-linux-gnu/bits/siginfo.h" 3 4
typedef struct siginfo
  {
    int si_signo;
    int si_errno;

    int si_code;

    union
      {
 int _pad[((128 / sizeof (int)) - 3)];


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
   } _kill;


 struct
   {
     int si_tid;
     int si_overrun;
     sigval_t si_sigval;
   } _timer;


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     sigval_t si_sigval;
   } _rt;


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     int si_status;
     __clock_t si_utime;
     __clock_t si_stime;
   } _sigchld;


 struct
   {
     void *si_addr;
   } _sigfault;


 struct
   {
     long int si_band;
     int si_fd;
   } _sigpoll;
      } _sifields;
  } siginfo_t;
# 129 "/usr/include/i386-linux-gnu/bits/siginfo.h" 3 4
enum
{
  SI_ASYNCNL = -60,

  SI_TKILL = -6,

  SI_SIGIO,

  SI_ASYNCIO,

  SI_MESGQ,

  SI_TIMER,

  SI_QUEUE,

  SI_USER,

  SI_KERNEL = 0x80

};



enum
{
  ILL_ILLOPC = 1,

  ILL_ILLOPN,

  ILL_ILLADR,

  ILL_ILLTRP,

  ILL_PRVOPC,

  ILL_PRVREG,

  ILL_COPROC,

  ILL_BADSTK

};


enum
{
  FPE_INTDIV = 1,

  FPE_INTOVF,

  FPE_FLTDIV,

  FPE_FLTOVF,

  FPE_FLTUND,

  FPE_FLTRES,

  FPE_FLTINV,

  FPE_FLTSUB

};


enum
{
  SEGV_MAPERR = 1,

  SEGV_ACCERR

};


enum
{
  BUS_ADRALN = 1,

  BUS_ADRERR,

  BUS_OBJERR

};


enum
{
  TRAP_BRKPT = 1,

  TRAP_TRACE

};


enum
{
  CLD_EXITED = 1,

  CLD_KILLED,

  CLD_DUMPED,

  CLD_TRAPPED,

  CLD_STOPPED,

  CLD_CONTINUED

};


enum
{
  POLL_IN = 1,

  POLL_OUT,

  POLL_MSG,

  POLL_ERR,

  POLL_PRI,

  POLL_HUP

};
# 273 "/usr/include/i386-linux-gnu/bits/siginfo.h" 3 4
typedef struct sigevent
  {
    sigval_t sigev_value;
    int sigev_signo;
    int sigev_notify;

    union
      {
 int _pad[((64 / sizeof (int)) - 3)];



 __pid_t _tid;

 struct
   {
     void (*_function) (sigval_t);
     void *_attribute;
   } _sigev_thread;
      } _sigev_un;
  } sigevent_t;






enum
{
  SIGEV_SIGNAL = 0,

  SIGEV_NONE,

  SIGEV_THREAD,


  SIGEV_THREAD_ID = 4

};
# 26 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h" 2
# 1 "/usr/include/i386-linux-gnu/bits/sigaction.h" 1 3 4
# 25 "/usr/include/i386-linux-gnu/bits/sigaction.h" 3 4
struct sigaction
  {


    union
      {

 __sighandler_t sa_handler;

 void (*sa_sigaction) (int, siginfo_t *, void *);
      }
    __sigaction_handler;







    __sigset_t sa_mask;


    int sa_flags;


    void (*sa_restorer) (void);
  };
# 27 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h" 2
# 1 "/usr/include/i386-linux-gnu/bits/sigstack.h" 1 3 4
# 26 "/usr/include/i386-linux-gnu/bits/sigstack.h" 3 4
struct sigstack
  {
    void *ss_sp;
    int ss_onstack;
  };



enum
{
  SS_ONSTACK = 1,

  SS_DISABLE

};
# 50 "/usr/include/i386-linux-gnu/bits/sigstack.h" 3 4
typedef struct sigaltstack
  {
    void *ss_sp;
    int ss_flags;
    size_t ss_size;
  } stack_t;
# 28 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h" 2
# 39 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/signal.h"
int kill (int, int);
void psignal (int, const char *);
int sigaction (int, const struct sigaction *, struct sigaction *);
int sigaddset (__sigset_t *, const int);
int sigdelset (__sigset_t *, const int);
int sigismember (const __sigset_t *, int);
int sigfillset (__sigset_t *);
int sigemptyset (__sigset_t *);
int sigpending (__sigset_t *);
int sigsuspend (const __sigset_t *);
int sigpause (int);
# 6 "/root/testvec/newlib-1.18.0/newlib/libc/include/signal.h" 2



typedef int sig_atomic_t;





struct _reent;

_sig_func_ptr _signal_r (struct _reent *, int, _sig_func_ptr);
int _raise_r (struct _reent *, int);


_sig_func_ptr signal (int, _sig_func_ptr);
int raise (int);



# 145 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h" 2



int clock_settime (clockid_t clock_id, const struct timespec *tp);
int clock_gettime (clockid_t clock_id, struct timespec *tp);
int clock_getres (clockid_t clock_id, struct timespec *res);



int timer_create (clockid_t clock_id, struct sigevent *evp, timer_t *timerid)
                                                               ;



int timer_delete (timer_t timerid);



int timer_settime (timer_t timerid, int flags, const struct itimerspec *value, struct itimerspec *ovalue)

                              ;
int timer_gettime (timer_t timerid, struct itimerspec *value);
int timer_getoverrun (timer_t timerid);



int nanosleep (const struct timespec *rqtp, struct timespec *rmtp);
# 226 "/root/testvec/newlib-1.18.0/newlib/targ-include/time.h"
int clock_getcpuclockid (pid_t pid, clockid_t *clock_id);







int clock_setenable_attr (clockid_t clock_id, int attr);
int clock_getenable_attr (clockid_t clock_id, int *attr);
# 22 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h" 2



# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/pthreadtypes.h" 1
# 23 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/pthreadtypes.h"
# 1 "/usr/include/i386-linux-gnu/bits/sched.h" 1 3 4
# 24 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/pthreadtypes.h" 2


struct _pthread_fastlock
{
  long int __status;
  int __spinlock;

};



typedef struct _pthread_descr_struct *_pthread_descr;





typedef struct __pthread_attr_s
{
  int __detachstate;
  int __schedpolicy;
  struct __sched_param __schedparam;
  int __inheritsched;
  int __scope;
  size_t __guardsize;
  int __stackaddr_set;
  void *__stackaddr;
  size_t __stacksize;
} pthread_attr_t;





__extension__ typedef long long __pthread_cond_align_t;




typedef struct
{
  struct _pthread_fastlock __c_lock;
  _pthread_descr __c_waiting;
  char __padding[48 - sizeof (struct _pthread_fastlock)
   - sizeof (_pthread_descr) - sizeof (__pthread_cond_align_t)];
  __pthread_cond_align_t __align;
} pthread_cond_t;



typedef struct
{
  int __dummy;
} pthread_condattr_t;


typedef unsigned int pthread_key_t;





typedef struct
{
  int __m_reserved;
  int __m_count;
  _pthread_descr __m_owner;
  int __m_kind;
  struct _pthread_fastlock __m_lock;
} pthread_mutex_t;



typedef struct
{
  int __mutexkind;
} pthread_mutexattr_t;



typedef int pthread_once_t;




typedef struct _pthread_rwlock_t
{
  struct _pthread_fastlock __rw_lock;
  int __rw_readers;
  _pthread_descr __rw_writer;
  _pthread_descr __rw_read_waiting;
  _pthread_descr __rw_write_waiting;
  int __rw_kind;
  int __rw_pshared;
} pthread_rwlock_t;



typedef struct
{
  int __lockkind;
  int __pshared;
} pthread_rwlockattr_t;




typedef volatile int pthread_spinlock_t;


typedef struct {
  struct _pthread_fastlock __ba_lock;
  int __ba_required;
  int __ba_present;
  _pthread_descr __ba_waiting;
} pthread_barrier_t;


typedef struct {
  int __pshared;
} pthread_barrierattr_t;





typedef unsigned long int pthread_t;
# 26 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/initspin.h" 1
# 27 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h" 2



# 59 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
enum
{
  PTHREAD_CREATE_JOINABLE,

  PTHREAD_CREATE_DETACHED

};

enum
{
  PTHREAD_INHERIT_SCHED,

  PTHREAD_EXPLICIT_SCHED

};

enum
{
  PTHREAD_SCOPE_SYSTEM,

  PTHREAD_SCOPE_PROCESS

};

enum
{
  PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_ADAPTIVE_NP
# 98 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
  , PTHREAD_MUTEX_FAST_NP = PTHREAD_MUTEX_ADAPTIVE_NP

};

enum
{
  PTHREAD_PROCESS_PRIVATE,

  PTHREAD_PROCESS_SHARED

};
# 131 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
struct _pthread_cleanup_buffer
{
  void (*__routine) (void *);
  void *__arg;
  int __canceltype;
  struct _pthread_cleanup_buffer *__prev;
};



enum
{
  PTHREAD_CANCEL_ENABLE,

  PTHREAD_CANCEL_DISABLE

};
enum
{
  PTHREAD_CANCEL_DEFERRED,

  PTHREAD_CANCEL_ASYNCHRONOUS

};
# 163 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_create (pthread_t *__restrict __thread1,
      __const pthread_attr_t *__restrict __attr,
      void *(*__start_routine) (void *),
      void *__restrict __arg) __attribute__ ((__nothrow__));


extern pthread_t pthread_self (void) __attribute__ ((__nothrow__));


extern int pthread_equal (pthread_t __thread1, pthread_t __thread2) __attribute__ ((__nothrow__));


extern void pthread_exit (void *__retval)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));




extern int pthread_join (pthread_t __th, void **__thread_return) __attribute__ ((__nothrow__));





extern int pthread_detach (pthread_t __th) __attribute__ ((__nothrow__));







extern int pthread_attr_init (pthread_attr_t *__attr) __attribute__ ((__nothrow__));


extern int pthread_attr_destroy (pthread_attr_t *__attr) __attribute__ ((__nothrow__));


extern int pthread_attr_setdetachstate (pthread_attr_t *__attr,
     int __detachstate) __attribute__ ((__nothrow__));


extern int pthread_attr_getdetachstate (__const pthread_attr_t *__attr,
     int *__detachstate) __attribute__ ((__nothrow__));


extern int pthread_attr_setschedparam (pthread_attr_t *__restrict __attr,
           __const struct sched_param *__restrict
           __param) __attribute__ ((__nothrow__));


extern int pthread_attr_getschedparam (__const pthread_attr_t *__restrict
           __attr,
           struct sched_param *__restrict __param)
     __attribute__ ((__nothrow__));


extern int pthread_attr_setschedpolicy (pthread_attr_t *__attr, int __policy)
     __attribute__ ((__nothrow__));


extern int pthread_attr_getschedpolicy (__const pthread_attr_t *__restrict
     __attr, int *__restrict __policy)
     __attribute__ ((__nothrow__));


extern int pthread_attr_setinheritsched (pthread_attr_t *__attr,
      int __inherit) __attribute__ ((__nothrow__));


extern int pthread_attr_getinheritsched (__const pthread_attr_t *__restrict
      __attr, int *__restrict __inherit)
     __attribute__ ((__nothrow__));


extern int pthread_attr_setscope (pthread_attr_t *__attr, int __scope)
     __attribute__ ((__nothrow__));


extern int pthread_attr_getscope (__const pthread_attr_t *__restrict __attr,
      int *__restrict __scope) __attribute__ ((__nothrow__));
# 260 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_attr_setstackaddr (pthread_attr_t *__attr,
          void *__stackaddr) __attribute__ ((__nothrow__));


extern int pthread_attr_getstackaddr (__const pthread_attr_t *__restrict
          __attr, void **__restrict __stackaddr)
     __attribute__ ((__nothrow__));





extern int pthread_attr_setstack (pthread_attr_t *__attr, void *__stackaddr,
      size_t __stacksize) __attribute__ ((__nothrow__));


extern int pthread_attr_getstack (__const pthread_attr_t *__restrict __attr,
      void **__restrict __stackaddr,
      size_t *__restrict __stacksize) __attribute__ ((__nothrow__));





extern int pthread_attr_setstacksize (pthread_attr_t *__attr,
          size_t __stacksize) __attribute__ ((__nothrow__));


extern int pthread_attr_getstacksize (__const pthread_attr_t *__restrict
          __attr, size_t *__restrict __stacksize)
     __attribute__ ((__nothrow__));



extern int pthread_getattr_np (pthread_t __th, pthread_attr_t *__attr) __attribute__ ((__nothrow__));






extern int pthread_setschedparam (pthread_t __target_thread, int __policy,
      __const struct sched_param *__param)
     __attribute__ ((__nothrow__));


extern int pthread_getschedparam (pthread_t __target_thread,
      int *__restrict __policy,
      struct sched_param *__restrict __param)
     __attribute__ ((__nothrow__));
# 324 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_yield (void) __attribute__ ((__nothrow__));






extern int pthread_mutex_init (pthread_mutex_t *__restrict __mutex,
          __const pthread_mutexattr_t *__restrict
          __mutex_attr) __attribute__ ((__nothrow__));


extern int pthread_mutex_destroy (pthread_mutex_t *__mutex) __attribute__ ((__nothrow__));


extern int pthread_mutex_trylock (pthread_mutex_t *__mutex) __attribute__ ((__nothrow__));


extern int pthread_mutex_lock (pthread_mutex_t *__mutex) __attribute__ ((__nothrow__));



extern int pthread_mutex_timedlock (pthread_mutex_t *__restrict __mutex,
        __const struct timespec *__restrict
        __abstime) __attribute__ ((__nothrow__));



extern int pthread_mutex_unlock (pthread_mutex_t *__mutex) __attribute__ ((__nothrow__));






extern int pthread_mutexattr_init (pthread_mutexattr_t *__attr) __attribute__ ((__nothrow__));


extern int pthread_mutexattr_destroy (pthread_mutexattr_t *__attr) __attribute__ ((__nothrow__));


extern int pthread_mutexattr_getpshared (__const pthread_mutexattr_t *
      __restrict __attr,
      int *__restrict __pshared) __attribute__ ((__nothrow__));


extern int pthread_mutexattr_setpshared (pthread_mutexattr_t *__attr,
      int __pshared) __attribute__ ((__nothrow__));
# 390 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_cond_init (pthread_cond_t *__restrict __cond,
         __const pthread_condattr_t *__restrict
         __cond_attr) __attribute__ ((__nothrow__));


extern int pthread_cond_destroy (pthread_cond_t *__cond) __attribute__ ((__nothrow__));


extern int pthread_cond_signal (pthread_cond_t *__cond) __attribute__ ((__nothrow__));


extern int pthread_cond_broadcast (pthread_cond_t *__cond) __attribute__ ((__nothrow__));



extern int pthread_cond_wait (pthread_cond_t *__restrict __cond,
         pthread_mutex_t *__restrict __mutex) __attribute__ ((__nothrow__));





extern int pthread_cond_timedwait (pthread_cond_t *__restrict __cond,
       pthread_mutex_t *__restrict __mutex,
       __const struct timespec *__restrict
       __abstime) __attribute__ ((__nothrow__));




extern int pthread_condattr_init (pthread_condattr_t *__attr) __attribute__ ((__nothrow__));


extern int pthread_condattr_destroy (pthread_condattr_t *__attr) __attribute__ ((__nothrow__));


extern int pthread_condattr_getpshared (__const pthread_condattr_t *
     __restrict __attr,
     int *__restrict __pshared) __attribute__ ((__nothrow__));


extern int pthread_condattr_setpshared (pthread_condattr_t *__attr,
     int __pshared) __attribute__ ((__nothrow__));
# 509 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_spin_init (pthread_spinlock_t *__lock, int __pshared)
     __attribute__ ((__nothrow__));


extern int pthread_spin_destroy (pthread_spinlock_t *__lock) __attribute__ ((__nothrow__));


extern int pthread_spin_lock (pthread_spinlock_t *__lock) __attribute__ ((__nothrow__));


extern int pthread_spin_trylock (pthread_spinlock_t *__lock) __attribute__ ((__nothrow__));


extern int pthread_spin_unlock (pthread_spinlock_t *__lock) __attribute__ ((__nothrow__));




extern int pthread_barrier_init (pthread_barrier_t *__restrict __barrier,
     __const pthread_barrierattr_t *__restrict
     __attr, unsigned int __count) __attribute__ ((__nothrow__));

extern int pthread_barrier_destroy (pthread_barrier_t *__barrier) __attribute__ ((__nothrow__));

extern int pthread_barrierattr_init (pthread_barrierattr_t *__attr) __attribute__ ((__nothrow__));

extern int pthread_barrierattr_destroy (pthread_barrierattr_t *__attr) __attribute__ ((__nothrow__));

extern int pthread_barrierattr_getpshared (__const pthread_barrierattr_t *
        __restrict __attr,
        int *__restrict __pshared) __attribute__ ((__nothrow__));

extern int pthread_barrierattr_setpshared (pthread_barrierattr_t *__attr,
        int __pshared) __attribute__ ((__nothrow__));

extern int pthread_barrier_wait (pthread_barrier_t *__barrier) __attribute__ ((__nothrow__));
# 556 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_key_create (pthread_key_t *__key,
          void (*__destr_function) (void *)) __attribute__ ((__nothrow__));


extern int pthread_key_delete (pthread_key_t __key) __attribute__ ((__nothrow__));


extern int pthread_setspecific (pthread_key_t __key,
    __const void *__pointer) __attribute__ ((__nothrow__));


extern void *pthread_getspecific (pthread_key_t __key) __attribute__ ((__nothrow__));
# 576 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_once (pthread_once_t *__once_control,
    void (*__init_routine) (void)) __attribute__ ((__nothrow__));






extern int pthread_setcancelstate (int __state, int *__oldstate) __attribute__ ((__nothrow__));



extern int pthread_setcanceltype (int __type, int *__oldtype) __attribute__ ((__nothrow__));


extern int pthread_cancel (pthread_t __thread1) __attribute__ ((__nothrow__));




extern void pthread_testcancel (void) __attribute__ ((__nothrow__));
# 610 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern void _pthread_cleanup_push (struct _pthread_cleanup_buffer *__buffer,
       void (*__routine) (void *),
       void *__arg) __attribute__ ((__nothrow__));







extern void _pthread_cleanup_pop (struct _pthread_cleanup_buffer *__buffer,
      int __execute) __attribute__ ((__nothrow__));
# 631 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern void _pthread_cleanup_push_defer (struct _pthread_cleanup_buffer *__buffer,
      void (*__routine) (void *),
      void *__arg) __attribute__ ((__nothrow__));
# 642 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern void _pthread_cleanup_pop_restore (struct _pthread_cleanup_buffer *__buffer,
       int __execute) __attribute__ ((__nothrow__));





extern int pthread_getcpuclockid (pthread_t __thread_id,
      clockid_t *__clock_id) __attribute__ ((__nothrow__));




# 1 "/usr/include/i386-linux-gnu/bits/sigthread.h" 1 3 4
# 31 "/usr/include/i386-linux-gnu/bits/sigthread.h" 3 4
extern int pthread_sigmask (int __how,
       __const __sigset_t *__restrict __newmask,
       __sigset_t *__restrict __oldmask)__attribute__ ((__nothrow__));


extern int pthread_kill (pthread_t __threadid, int __signo) __attribute__ ((__nothrow__));



extern int pthread_sigqueue (pthread_t __threadid, int __signo,
        const union sigval __value) __attribute__ ((__nothrow__));
# 656 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h" 2
# 671 "/root/testvec/newlib-1.18.0/newlib/targ-include/pthread.h"
extern int pthread_atfork (void (*__prepare) (void),
      void (*__parent) (void),
      void (*__child) (void)) __attribute__ ((__nothrow__));




extern void pthread_kill_other_threads_np (void) __attribute__ ((__nothrow__));


# 25 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/libc-lock.h" 2
# 34 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/libc-lock.h"
typedef pthread_mutex_t __libc_lock_t;
typedef struct { pthread_mutex_t mutex; } __libc_lock_recursive_t;



typedef struct __libc_rwlock_opaque__ __libc_rwlock_t;

typedef __libc_lock_recursive_t __rtld_lock_recursive_t;







typedef pthread_key_t __libc_key_t;
# 297 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/libc-lock.h"
extern int __pthread_mutex_init (pthread_mutex_t *__mutex,
     __const pthread_mutexattr_t *__mutex_attr);

extern int __pthread_mutex_destroy (pthread_mutex_t *__mutex);

extern int __pthread_mutex_trylock (pthread_mutex_t *__mutex);

extern int __pthread_mutex_lock (pthread_mutex_t *__mutex);

extern int __pthread_mutex_unlock (pthread_mutex_t *__mutex);

extern int __pthread_mutexattr_init (pthread_mutexattr_t *__attr);

extern int __pthread_mutexattr_destroy (pthread_mutexattr_t *__attr);

extern int __pthread_mutexattr_settype (pthread_mutexattr_t *__attr,
     int __kind);
# 332 "/root/testvec/newlib-1.18.0/newlib/targ-include/bits/libc-lock.h"
extern int __pthread_key_create (pthread_key_t *__key,
     void (*__destr_function) (void *));

extern int __pthread_setspecific (pthread_key_t __key,
      __const void *__pointer);

extern void *__pthread_getspecific (pthread_key_t __key);

extern int __pthread_once (pthread_once_t *__once_control,
      void (*__init_routine) (void));

extern int __pthread_atfork (void (*__prepare) (void),
        void (*__parent) (void),
        void (*__child) (void));






#pragma weak __pthread_mutex_init
#pragma weak __pthread_mutex_destroy
#pragma weak __pthread_mutex_lock
#pragma weak __pthread_mutex_trylock
#pragma weak __pthread_mutex_unlock
#pragma weak __pthread_mutexattr_init
#pragma weak __pthread_mutexattr_destroy
#pragma weak __pthread_mutexattr_settype
#pragma weak __pthread_rwlock_destroy
#pragma weak __pthread_rwlock_rdlock
#pragma weak __pthread_rwlock_tryrdlock
#pragma weak __pthread_rwlock_wrlock
#pragma weak __pthread_rwlock_trywrlock
#pragma weak __pthread_rwlock_unlock
#pragma weak __pthread_key_create
#pragma weak __pthread_setspecific
#pragma weak __pthread_getspecific
#pragma weak __pthread_once
#pragma weak __pthread_initialize
#pragma weak __pthread_atfork
#pragma weak _pthread_cleanup_push_defer
#pragma weak _pthread_cleanup_pop_restore
#pragma weak _pthread_cleanup_push
#pragma weak _pthread_cleanup_pop
# 14 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/lock.h" 2

typedef __libc_lock_t _LOCK_T;
typedef __libc_lock_recursive_t _LOCK_RECURSIVE_T;
# 160 "./vfprintf.c" 2
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stdarg.h" 1 3 4
# 102 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stdarg.h" 3 4
typedef __gnuc_va_list va_list;
# 161 "./vfprintf.c" 2
# 1 "./local.h" 1
# 29 "./local.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/unistd.h" 1
# 10 "/root/testvec/newlib-1.18.0/newlib/targ-include/unistd.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h" 1
# 15 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h"
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/stddef.h" 1 3 4
# 16 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h" 2

extern char **environ;

void _exit (int __status ) __attribute__ ((noreturn));

int access (const char *__path, int __amode );
unsigned alarm (unsigned __secs );
int chdir (const char *__path );
int chmod (const char *__path, mode_t __mode );
int chown (const char *__path, uid_t __owner, gid_t __group );
int chroot (const char *__path );
int close (int __fildes );
char *ctermid (char *__s );
char *cuserid (char *__s );
int dup (int __fildes );
int dup2 (int __fildes, int __fildes2 );
int execl (const char *__path, const char *, ... );
int execle (const char *__path, const char *, ... );
int execlp (const char *__file, const char *, ... );
int execv (const char *__path, char * const __argv[] );
int execve (const char *__path, char * const __argv[], char * const __envp[] );
int execvp (const char *__file, char * const __argv[] );
int fchdir (int __fildes);
int fchmod (int __fildes, mode_t __mode );
int fchown (int __fildes, uid_t __owner, gid_t __group );
pid_t fork (void );
long fpathconf (int __fd, int __name );
int fsync (int __fd);
int ftruncate (int __fd, off_t __length);
char *getcwd (char *__buf, size_t __size );
int getdomainname (char *__name, size_t __len);
gid_t getegid (void );
uid_t geteuid (void );
gid_t getgid (void );
int getgroups (int __gidsetsize, gid_t __grouplist[] );
int __gethostname (char *__name, size_t __len);
char *getlogin (void );

int getlogin_r (char *name, size_t namesize);

char *getpass (__const char *__prompt);
int getpagesize (void);
pid_t getpgid (pid_t);
pid_t getpgrp (void );
pid_t getpid (void );
pid_t getppid (void );
uid_t getuid (void );
char * getusershell (void);
char *getwd (char *__buf );
int isatty (int __fildes );
int lchown (const char *__path, uid_t __owner, gid_t __group );
int link (const char *__path1, const char *__path2 );
int nice (int __nice_value );
off_t lseek (int __fildes, off_t __offset, int __whence );
long pathconf (const char *__path, int __name );
int pause (void );
int pipe (int __fildes[2] );
ssize_t pread (int __fd, void *__buf, size_t __nbytes, off_t __offset);
ssize_t pwrite (int __fd, const void *__buf, size_t __nbytes, off_t __offset);
_ssize_t read (int __fd, void *__buf, size_t __nbyte );
int readlink (const char *path, char *buf, size_t bufsiz);
int rmdir (const char *__path );
void * sbrk (ptrdiff_t __incr);
int setegid (gid_t __gid );
int seteuid (uid_t __uid );
int setgid (gid_t __gid );
int setpgid (pid_t __pid, pid_t __pgid );
int setpgrp (void );
pid_t setsid (void );
int setuid (uid_t __uid );
unsigned sleep (unsigned int __seconds );
void swab (const void *, void *, ssize_t);
int symlink (const char *oldpath, const char *newpath);
long sysconf (int __name );
pid_t tcgetpgrp (int __fildes );
int tcsetpgrp (int __fildes, pid_t __pgrp_id );
int truncate (const char *, off_t __length);
char * ttyname (int __fildes );
int ttyname_r (int __fildes, char *__buf, size_t __len);
int unlink (const char *__path );
int usleep (__useconds_t __useconds);
int vhangup (void );
_ssize_t write (int __fd, const void *__buf, size_t __nbyte );

extern char *optarg;
extern int optind, opterr, optopt;
int getopt(int, char * const [], const char *);
extern int optreset;
# 114 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h"
int _close (int __fildes );
pid_t _fork (void );
pid_t _getpid (void );
int _link (const char *__path1, const char *__path2 );
off_t _lseek (int __fildes, off_t __offset, int __whence );
_ssize_t _read (int __fd, void *__buf, size_t __nbyte );
void * _sbrk (size_t __incr);
int _unlink (const char *__path );
_ssize_t _write (int __fd, const void *__buf, size_t __nbyte );
int _execve (const char *__path, char * const __argv[], char * const __envp[] );
# 140 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h"
# 1 "/usr/include/i386-linux-gnu/bits/environments.h" 1 3 4
# 141 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h" 2
# 1 "/usr/include/i386-linux-gnu/bits/confname.h" 1 3 4
# 26 "/usr/include/i386-linux-gnu/bits/confname.h" 3 4
enum
  {
    _PC_LINK_MAX,

    _PC_MAX_CANON,

    _PC_MAX_INPUT,

    _PC_NAME_MAX,

    _PC_PATH_MAX,

    _PC_PIPE_BUF,

    _PC_CHOWN_RESTRICTED,

    _PC_NO_TRUNC,

    _PC_VDISABLE,

    _PC_SYNC_IO,

    _PC_ASYNC_IO,

    _PC_PRIO_IO,

    _PC_SOCK_MAXBUF,

    _PC_FILESIZEBITS,

    _PC_REC_INCR_XFER_SIZE,

    _PC_REC_MAX_XFER_SIZE,

    _PC_REC_MIN_XFER_SIZE,

    _PC_REC_XFER_ALIGN,

    _PC_ALLOC_SIZE_MIN,

    _PC_SYMLINK_MAX,

    _PC_2_SYMLINKS

  };


enum
  {
    _SC_ARG_MAX,

    _SC_CHILD_MAX,

    _SC_CLK_TCK,

    _SC_NGROUPS_MAX,

    _SC_OPEN_MAX,

    _SC_STREAM_MAX,

    _SC_TZNAME_MAX,

    _SC_JOB_CONTROL,

    _SC_SAVED_IDS,

    _SC_REALTIME_SIGNALS,

    _SC_PRIORITY_SCHEDULING,

    _SC_TIMERS,

    _SC_ASYNCHRONOUS_IO,

    _SC_PRIORITIZED_IO,

    _SC_SYNCHRONIZED_IO,

    _SC_FSYNC,

    _SC_MAPPED_FILES,

    _SC_MEMLOCK,

    _SC_MEMLOCK_RANGE,

    _SC_MEMORY_PROTECTION,

    _SC_MESSAGE_PASSING,

    _SC_SEMAPHORES,

    _SC_SHARED_MEMORY_OBJECTS,

    _SC_AIO_LISTIO_MAX,

    _SC_AIO_MAX,

    _SC_AIO_PRIO_DELTA_MAX,

    _SC_DELAYTIMER_MAX,

    _SC_MQ_OPEN_MAX,

    _SC_MQ_PRIO_MAX,

    _SC_VERSION,

    _SC_PAGESIZE,


    _SC_RTSIG_MAX,

    _SC_SEM_NSEMS_MAX,

    _SC_SEM_VALUE_MAX,

    _SC_SIGQUEUE_MAX,

    _SC_TIMER_MAX,




    _SC_BC_BASE_MAX,

    _SC_BC_DIM_MAX,

    _SC_BC_SCALE_MAX,

    _SC_BC_STRING_MAX,

    _SC_COLL_WEIGHTS_MAX,

    _SC_EQUIV_CLASS_MAX,

    _SC_EXPR_NEST_MAX,

    _SC_LINE_MAX,

    _SC_RE_DUP_MAX,

    _SC_CHARCLASS_NAME_MAX,


    _SC_2_VERSION,

    _SC_2_C_BIND,

    _SC_2_C_DEV,

    _SC_2_FORT_DEV,

    _SC_2_FORT_RUN,

    _SC_2_SW_DEV,

    _SC_2_LOCALEDEF,


    _SC_PII,

    _SC_PII_XTI,

    _SC_PII_SOCKET,

    _SC_PII_INTERNET,

    _SC_PII_OSI,

    _SC_POLL,

    _SC_SELECT,

    _SC_UIO_MAXIOV,

    _SC_IOV_MAX = _SC_UIO_MAXIOV,

    _SC_PII_INTERNET_STREAM,

    _SC_PII_INTERNET_DGRAM,

    _SC_PII_OSI_COTS,

    _SC_PII_OSI_CLTS,

    _SC_PII_OSI_M,

    _SC_T_IOV_MAX,



    _SC_THREADS,

    _SC_THREAD_SAFE_FUNCTIONS,

    _SC_GETGR_R_SIZE_MAX,

    _SC_GETPW_R_SIZE_MAX,

    _SC_LOGIN_NAME_MAX,

    _SC_TTY_NAME_MAX,

    _SC_THREAD_DESTRUCTOR_ITERATIONS,

    _SC_THREAD_KEYS_MAX,

    _SC_THREAD_STACK_MIN,

    _SC_THREAD_THREADS_MAX,

    _SC_THREAD_ATTR_STACKADDR,

    _SC_THREAD_ATTR_STACKSIZE,

    _SC_THREAD_PRIORITY_SCHEDULING,

    _SC_THREAD_PRIO_INHERIT,

    _SC_THREAD_PRIO_PROTECT,

    _SC_THREAD_PROCESS_SHARED,


    _SC_NPROCESSORS_CONF,

    _SC_NPROCESSORS_ONLN,

    _SC_PHYS_PAGES,

    _SC_AVPHYS_PAGES,

    _SC_ATEXIT_MAX,

    _SC_PASS_MAX,


    _SC_XOPEN_VERSION,

    _SC_XOPEN_XCU_VERSION,

    _SC_XOPEN_UNIX,

    _SC_XOPEN_CRYPT,

    _SC_XOPEN_ENH_I18N,

    _SC_XOPEN_SHM,


    _SC_2_CHAR_TERM,

    _SC_2_C_VERSION,

    _SC_2_UPE,


    _SC_XOPEN_XPG2,

    _SC_XOPEN_XPG3,

    _SC_XOPEN_XPG4,


    _SC_CHAR_BIT,

    _SC_CHAR_MAX,

    _SC_CHAR_MIN,

    _SC_INT_MAX,

    _SC_INT_MIN,

    _SC_LONG_BIT,

    _SC_WORD_BIT,

    _SC_MB_LEN_MAX,

    _SC_NZERO,

    _SC_SSIZE_MAX,

    _SC_SCHAR_MAX,

    _SC_SCHAR_MIN,

    _SC_SHRT_MAX,

    _SC_SHRT_MIN,

    _SC_UCHAR_MAX,

    _SC_UINT_MAX,

    _SC_ULONG_MAX,

    _SC_USHRT_MAX,


    _SC_NL_ARGMAX,

    _SC_NL_LANGMAX,

    _SC_NL_MSGMAX,

    _SC_NL_NMAX,

    _SC_NL_SETMAX,

    _SC_NL_TEXTMAX,


    _SC_XBS5_ILP32_OFF32,

    _SC_XBS5_ILP32_OFFBIG,

    _SC_XBS5_LP64_OFF64,

    _SC_XBS5_LPBIG_OFFBIG,


    _SC_XOPEN_LEGACY,

    _SC_XOPEN_REALTIME,

    _SC_XOPEN_REALTIME_THREADS,


    _SC_ADVISORY_INFO,

    _SC_BARRIERS,

    _SC_BASE,

    _SC_C_LANG_SUPPORT,

    _SC_C_LANG_SUPPORT_R,

    _SC_CLOCK_SELECTION,

    _SC_CPUTIME,

    _SC_THREAD_CPUTIME,

    _SC_DEVICE_IO,

    _SC_DEVICE_SPECIFIC,

    _SC_DEVICE_SPECIFIC_R,

    _SC_FD_MGMT,

    _SC_FIFO,

    _SC_PIPE,

    _SC_FILE_ATTRIBUTES,

    _SC_FILE_LOCKING,

    _SC_FILE_SYSTEM,

    _SC_MONOTONIC_CLOCK,

    _SC_MULTI_PROCESS,

    _SC_SINGLE_PROCESS,

    _SC_NETWORKING,

    _SC_READER_WRITER_LOCKS,

    _SC_SPIN_LOCKS,

    _SC_REGEXP,

    _SC_REGEX_VERSION,

    _SC_SHELL,

    _SC_SIGNALS,

    _SC_SPAWN,

    _SC_SPORADIC_SERVER,

    _SC_THREAD_SPORADIC_SERVER,

    _SC_SYSTEM_DATABASE,

    _SC_SYSTEM_DATABASE_R,

    _SC_TIMEOUTS,

    _SC_TYPED_MEMORY_OBJECTS,

    _SC_USER_GROUPS,

    _SC_USER_GROUPS_R,

    _SC_2_PBS,

    _SC_2_PBS_ACCOUNTING,

    _SC_2_PBS_LOCATE,

    _SC_2_PBS_MESSAGE,

    _SC_2_PBS_TRACK,

    _SC_SYMLOOP_MAX,

    _SC_STREAMS,

    _SC_2_PBS_CHECKPOINT,


    _SC_V6_ILP32_OFF32,

    _SC_V6_ILP32_OFFBIG,

    _SC_V6_LP64_OFF64,

    _SC_V6_LPBIG_OFFBIG,


    _SC_HOST_NAME_MAX,

    _SC_TRACE,

    _SC_TRACE_EVENT_FILTER,

    _SC_TRACE_INHERIT,

    _SC_TRACE_LOG,


    _SC_LEVEL1_ICACHE_SIZE,

    _SC_LEVEL1_ICACHE_ASSOC,

    _SC_LEVEL1_ICACHE_LINESIZE,

    _SC_LEVEL1_DCACHE_SIZE,

    _SC_LEVEL1_DCACHE_ASSOC,

    _SC_LEVEL1_DCACHE_LINESIZE,

    _SC_LEVEL2_CACHE_SIZE,

    _SC_LEVEL2_CACHE_ASSOC,

    _SC_LEVEL2_CACHE_LINESIZE,

    _SC_LEVEL3_CACHE_SIZE,

    _SC_LEVEL3_CACHE_ASSOC,

    _SC_LEVEL3_CACHE_LINESIZE,

    _SC_LEVEL4_CACHE_SIZE,

    _SC_LEVEL4_CACHE_ASSOC,

    _SC_LEVEL4_CACHE_LINESIZE,



    _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,

    _SC_RAW_SOCKETS,


    _SC_V7_ILP32_OFF32,

    _SC_V7_ILP32_OFFBIG,

    _SC_V7_LP64_OFF64,

    _SC_V7_LPBIG_OFFBIG,


    _SC_SS_REPL_MAX,


    _SC_TRACE_EVENT_NAME_MAX,

    _SC_TRACE_NAME_MAX,

    _SC_TRACE_SYS_MAX,

    _SC_TRACE_USER_EVENT_MAX,


    _SC_XOPEN_STREAMS,


    _SC_THREAD_ROBUST_PRIO_INHERIT,

    _SC_THREAD_ROBUST_PRIO_PROTECT

  };


enum
  {
    _CS_PATH,


    _CS_V6_WIDTH_RESTRICTED_ENVS,



    _CS_GNU_LIBC_VERSION,

    _CS_GNU_LIBPTHREAD_VERSION,


    _CS_V5_WIDTH_RESTRICTED_ENVS,



    _CS_V7_WIDTH_RESTRICTED_ENVS,



    _CS_LFS_CFLAGS = 1000,

    _CS_LFS_LDFLAGS,

    _CS_LFS_LIBS,

    _CS_LFS_LINTFLAGS,

    _CS_LFS64_CFLAGS,

    _CS_LFS64_LDFLAGS,

    _CS_LFS64_LIBS,

    _CS_LFS64_LINTFLAGS,


    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,

    _CS_XBS5_ILP32_OFF32_LDFLAGS,

    _CS_XBS5_ILP32_OFF32_LIBS,

    _CS_XBS5_ILP32_OFF32_LINTFLAGS,

    _CS_XBS5_ILP32_OFFBIG_CFLAGS,

    _CS_XBS5_ILP32_OFFBIG_LDFLAGS,

    _CS_XBS5_ILP32_OFFBIG_LIBS,

    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,

    _CS_XBS5_LP64_OFF64_CFLAGS,

    _CS_XBS5_LP64_OFF64_LDFLAGS,

    _CS_XBS5_LP64_OFF64_LIBS,

    _CS_XBS5_LP64_OFF64_LINTFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_CFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_LIBS,

    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,


    _CS_POSIX_V6_ILP32_OFF32_CFLAGS,

    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,

    _CS_POSIX_V6_ILP32_OFF32_LIBS,

    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_LIBS,

    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,

    _CS_POSIX_V6_LP64_OFF64_CFLAGS,

    _CS_POSIX_V6_LP64_OFF64_LDFLAGS,

    _CS_POSIX_V6_LP64_OFF64_LIBS,

    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS,


    _CS_POSIX_V7_ILP32_OFF32_CFLAGS,

    _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,

    _CS_POSIX_V7_ILP32_OFF32_LIBS,

    _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_LIBS,

    _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS,

    _CS_POSIX_V7_LP64_OFF64_CFLAGS,

    _CS_POSIX_V7_LP64_OFF64_LDFLAGS,

    _CS_POSIX_V7_LP64_OFF64_LIBS,

    _CS_POSIX_V7_LP64_OFF64_LINTFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS,


    _CS_V6_ENV,

    _CS_V7_ENV

  };
# 142 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/unistd.h" 2
# 11 "/root/testvec/newlib-1.18.0/newlib/targ-include/unistd.h" 2



extern char *optarg;
extern int optind, opterr, optopt;
# 30 "./local.h" 2






extern u_char *__sccl (char *, u_char *fmt);
extern int __svfscanf_r (struct _reent *,FILE *, const char *,va_list);
extern int __ssvfscanf_r (struct _reent *,FILE *, const char *,va_list);
extern int __svfiscanf_r (struct _reent *,FILE *, const char *,va_list);
extern int __ssvfiscanf_r (struct _reent *,FILE *, const char *,va_list);
extern int __svfwscanf_r (struct _reent *,FILE *, const wchar_t *,va_list);
extern int __ssvfwscanf_r (struct _reent *,FILE *, const wchar_t *,va_list);
extern int __svfiwscanf_r (struct _reent *,FILE *, const wchar_t *,va_list);
extern int __ssvfiwscanf_r (struct _reent *,FILE *, const wchar_t *,va_list);
int _svfprintf_r (struct _reent *, FILE *, const char *, va_list) __attribute__ ((__format__ (__printf__, 3, 0)))

                                                               ;
int _svfiprintf_r (struct _reent *, FILE *, const char *, va_list) __attribute__ ((__format__ (__printf__, 3, 0)))

                                                               ;
int _svfwprintf_r (struct _reent *, FILE *, const wchar_t *, va_list)
               ;
int _svfiwprintf_r (struct _reent *, FILE *, const wchar_t *, va_list)
               ;
extern FILE *__sfp (struct _reent *);
extern int __sflags (struct _reent *,const char*, int*);
extern int __srefill_r (struct _reent *,FILE *);
extern _ssize_t __sread (struct _reent *, void *, char *, int)
                 ;
extern _ssize_t __seofread (struct _reent *, void *, char *, int)
                     ;
extern _ssize_t __swrite (struct _reent *, void *, const char *, int)
                         ;
extern _fpos_t __sseek (struct _reent *, void *, _fpos_t, int);
extern int __sclose (struct _reent *, void *);
extern int __stextmode (int);
extern void __sinit (struct _reent *);
extern void _cleanup_r (struct _reent *);
extern void __smakebuf_r (struct _reent *, FILE *);
extern int _fwalk (struct _reent *, int (*)(FILE *));
extern int _fwalk_reent (struct _reent *, int (*)(struct _reent *, FILE *));
struct _glue * __sfmoreglue (struct _reent *,int n);
extern int __submore (struct _reent *, FILE *);


extern _fpos64_t __sseek64 (struct _reent *, void *, _fpos64_t, int);
extern _ssize_t __swrite64 (struct _reent *, void *, const char *, int)
                           ;
# 157 "./local.h"
char *_dcvt (struct _reent *, char *, double, int, int, char, int);
char *_sicvt (char *, short, char);
char *_icvt (char *, int, char);
char *_licvt (char *, long, char);

char *_llicvt (char *, long long, char);
# 175 "./local.h"
void __sfp_lock_acquire (void);
void __sfp_lock_release (void);
void __sinit_lock_acquire (void);
void __sinit_lock_release (void);





typedef enum {
  ZERO,
  DIGIT,
  DOLLAR,
  MODFR,
  SPEC,
  DOT,
  STAR,
  FLAG,
  OTHER,
  MAX_CH_CLASS
} __CH_CLASS;

typedef enum {
  START,
  SFLAG,
  WDIG,
  WIDTH,
  SMOD,
  SDOT,
  VARW,
  VARP,
  PREC,
  VWDIG,
  VPDIG,
  DONE,
  MAX_STATE,
} __STATE;

typedef enum {
  NOOP,
  NUMBER,
  SKIPNUM,
  GETMOD,
  GETARG,
  GETPW,
  GETPWB,
  GETPOS,
  PWPOS,
} __ACTION;

extern const __CH_CLASS __chclass[256];
extern const __STATE __state_table[MAX_STATE][MAX_CH_CLASS];
extern const __ACTION __action_table[MAX_STATE][MAX_CH_CLASS];
# 162 "./vfprintf.c" 2
# 1 "./../stdlib/local.h" 1





char * _gcvt (struct _reent *, double , int , char *, char, int);

char *__locale_charset(void);





extern int (*__wctomb) (struct _reent *, char *, wchar_t, const char *,
   mbstate_t *);
int __ascii_wctomb (struct _reent *, char *, wchar_t, const char *,
      mbstate_t *);

int __utf8_wctomb (struct _reent *, char *, wchar_t, const char *, mbstate_t *);
int __sjis_wctomb (struct _reent *, char *, wchar_t, const char *, mbstate_t *);
int __eucjp_wctomb (struct _reent *, char *, wchar_t, const char *,
      mbstate_t *);
int __jis_wctomb (struct _reent *, char *, wchar_t, const char *, mbstate_t *);
int __iso_wctomb (struct _reent *, char *, wchar_t, const char *, mbstate_t *);
int __cp_wctomb (struct _reent *, char *, wchar_t, const char *, mbstate_t *);







extern int (*__mbtowc) (struct _reent *, wchar_t *, const char *, size_t,
   const char *, mbstate_t *);
int __ascii_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
      const char *, mbstate_t *);

int __utf8_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
     const char *, mbstate_t *);
int __sjis_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
     const char *, mbstate_t *);
int __eucjp_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
      const char *, mbstate_t *);
int __jis_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
    const char *, mbstate_t *);
int __iso_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
    const char *, mbstate_t *);
int __cp_mbtowc (struct _reent *, wchar_t *, const char *, size_t,
   const char *, mbstate_t *);
# 60 "./../stdlib/local.h"
extern wchar_t __iso_8859_conv[14][0x60];
int __iso_8859_index (const char *);

extern wchar_t __cp_conv[][0x80];
int __cp_index (const char *);
# 163 "./vfprintf.c" 2
# 1 "./fvwrite.h" 1
# 24 "./fvwrite.h"
struct __siov {
 const void * iov_base;
 size_t iov_len;
};
struct __suio {
 struct __siov *uio_iov;
 int uio_iovcnt;
 int uio_resid;
};


extern int __sfvwrite_r (struct _reent *, FILE *, struct __suio *);
extern int __swsetup_r (struct _reent *, FILE *);
# 164 "./vfprintf.c" 2
# 1 "./vfieeefp.h" 1
# 33 "./vfieeefp.h"
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/ieeefp.h" 1





# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/ieeefp.h" 1
# 7 "/root/testvec/newlib-1.18.0/newlib/libc/include/ieeefp.h" 2


# 53 "/root/testvec/newlib-1.18.0/newlib/libc/include/ieeefp.h"
typedef union
{
  double value;
  struct
  {






    unsigned int fraction1:32;
    unsigned int fraction0:20;

    unsigned int exponent :11;
    unsigned int sign : 1;
  } number;
  struct
  {






    unsigned int function1:32;
    unsigned int function0:19;

    unsigned int quiet:1;
    unsigned int exponent: 11;
    unsigned int sign : 1;
  } nan;
  struct
  {
    unsigned long lsw;
    unsigned long msw;
  } parts;

  long aslong[2];

} __ieee_double_shape_type;
# 125 "/root/testvec/newlib-1.18.0/newlib/libc/include/ieeefp.h"
typedef union
{
  float value;
  struct
  {
    unsigned int fraction0: 7;
    unsigned int fraction1: 16;
    unsigned int exponent: 8;
    unsigned int sign : 1;
  } number;
  struct
  {
    unsigned int function1:16;
    unsigned int function0:6;
    unsigned int quiet:1;
    unsigned int exponent:8;
    unsigned int sign:1;
  } nan;
  long p1;

} __ieee_float_shape_type;
# 155 "/root/testvec/newlib-1.18.0/newlib/libc/include/ieeefp.h"
typedef int fp_rnd;





fp_rnd fpgetround (void);
fp_rnd fpsetround (fp_rnd);



typedef int fp_except;






fp_except fpgetmask (void);
fp_except fpsetmask (fp_except);
fp_except fpgetsticky (void);
fp_except fpsetsticky (fp_except);



typedef int fp_rdi;



fp_rdi fpgetroundtoi (void);
fp_rdi fpsetroundtoi (fp_rdi);




int isnan (double);
int isinf (double);
int finite (double);



int isnanf (float);
int isinff (float);
int finitef (float);
# 254 "/root/testvec/newlib-1.18.0/newlib/libc/include/ieeefp.h"

# 34 "./vfieeefp.h" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h" 1





# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/machine/ieeefp.h" 1
# 7 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h" 2







union __dmath
{
  double d;
  __ULong i[2];
};

union __fmath
{
  float f;
  __ULong i[1];
};


union __ldmath
{
  long double ld;
  __ULong i[4];
};
# 111 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
extern double atan (double);
extern double cos (double);
extern double sin (double);
extern double tan (double);
extern double tanh (double);
extern double frexp (double, int *);
extern double modf (double, double *);
extern double ceil (double);
extern double fabs (double);
extern double floor (double);






extern double acos (double);
extern double asin (double);
extern double atan2 (double, double);
extern double cosh (double);
extern double sinh (double);
extern double exp (double);
extern double ldexp (double, int);
extern double log (double);
extern double log10 (double);
extern double pow (double, double);
extern double sqrt (double);
extern double fmod (double, double);
# 148 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
typedef float float_t;
typedef double double_t;
# 175 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
extern int __isinff (float x);
extern int __isinfd (double x);
extern int __isnanf (float x);
extern int __isnand (double x);
extern int __fpclassifyf (float x);
extern int __fpclassifyd (double x);
extern int __signbitf (float x);
extern int __signbitd (double x);
# 235 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
extern double infinity (void);
extern double nan (const char *);
extern int finite (double);
extern double copysign (double, double);
extern double logb (double);
extern int ilogb (double);

extern double asinh (double);
extern double cbrt (double);
extern double nextafter (double, double);
extern double rint (double);
extern double scalbn (double, int);

extern double exp2 (double);
extern double scalbln (double, long int);
extern double tgamma (double);
extern double nearbyint (double);
extern long int lrint (double);
extern long long int llrint (double);
extern double round (double);
extern long int lround (double);
extern long long int llround (double);
extern double trunc (double);
extern double remquo (double, double, int *);
extern double fdim (double, double);
extern double fmax (double, double);
extern double fmin (double, double);
extern double fma (double, double, double);


extern double log1p (double);
extern double expm1 (double);



extern double acosh (double);
extern double atanh (double);
extern double remainder (double, double);
extern double gamma (double);
extern double lgamma (double);
extern double erf (double);
extern double erfc (double);
extern double log2 (double);



extern double hypot (double, double);






extern float atanf (float);
extern float cosf (float);
extern float sinf (float);
extern float tanf (float);
extern float tanhf (float);
extern float frexpf (float, int *);
extern float modff (float, float *);
extern float ceilf (float);
extern float fabsf (float);
extern float floorf (float);


extern float acosf (float);
extern float asinf (float);
extern float atan2f (float, float);
extern float coshf (float);
extern float sinhf (float);
extern float expf (float);
extern float ldexpf (float, int);
extern float logf (float);
extern float log10f (float);
extern float powf (float, float);
extern float sqrtf (float);
extern float fmodf (float, float);




extern float exp2f (float);
extern float scalblnf (float, long int);
extern float tgammaf (float);
extern float nearbyintf (float);
extern long int lrintf (float);
extern long long llrintf (float);
extern float roundf (float);
extern long int lroundf (float);
extern long long int llroundf (float);
extern float truncf (float);
extern float remquof (float, float, int *);
extern float fdimf (float, float);
extern float fmaxf (float, float);
extern float fminf (float, float);
extern float fmaf (float, float, float);

extern float infinityf (void);
extern float nanf (const char *);
extern int finitef (float);
extern float copysignf (float, float);
extern float logbf (float);
extern int ilogbf (float);

extern float asinhf (float);
extern float cbrtf (float);
extern float nextafterf (float, float);
extern float rintf (float);
extern float scalbnf (float, int);
extern float log1pf (float);
extern float expm1f (float);


extern float acoshf (float);
extern float atanhf (float);
extern float remainderf (float, float);
extern float gammaf (float);
extern float lgammaf (float);
extern float erff (float);
extern float erfcf (float);
extern float log2f (float);

extern float hypotf (float, float);
# 429 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
extern long double rintl (long double);
extern long int lrintl (long double);
extern long long llrintl (long double);







extern double cabs();
extern double drem (double, double);
extern void sincos (double, double *, double *);
extern double gamma_r (double, int *);
extern double lgamma_r (double, int *);

extern double y0 (double);
extern double y1 (double);
extern double yn (int, double);
extern double j0 (double);
extern double j1 (double);
extern double jn (int, double);

extern float cabsf();
extern float dremf (float, float);
extern void sincosf (float, float *, float *);
extern float gammaf_r (float, int *);
extern float lgammaf_r (float, int *);

extern float y0f (float);
extern float y1f (float);
extern float ynf (int, float);
extern float j0f (float);
extern float j1f (float);
extern float jnf (int, float);



extern double exp10 (double);


extern double pow10 (double);


extern float exp10f (float);


extern float pow10f (float);
# 486 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
extern int *__signgam (void);
# 497 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
struct exception

{
  int type;
  char *name;
  double arg1;
  double arg2;
  double retval;
  int err;
};




extern int matherr (struct exception *e);
# 552 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"
enum __fdlibm_version
{
  __fdlibm_ieee = -1,
  __fdlibm_svid,
  __fdlibm_xopen,
  __fdlibm_posix
};




extern enum __fdlibm_version __fdlib_version;
# 572 "/root/testvec/newlib-1.18.0/newlib/libc/include/math.h"

# 35 "./vfieeefp.h" 2
# 1 "/usr/lib/gcc/i486-linux-gnu/4.7/include/float.h" 1 3 4
# 36 "./vfieeefp.h" 2
# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/errno.h" 1




typedef int error_t;



# 1 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/errno.h" 1
# 15 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/errno.h"
extern int *__errno (void);


extern const char * const _sys_errlist[];
extern int _sys_nerr;
# 28 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/errno.h"
# 1 "/usr/include/i386-linux-gnu/asm/errno.h" 1 3 4
# 1 "/usr/include/asm-generic/errno.h" 1 3 4



# 1 "/usr/include/asm-generic/errno-base.h" 1 3 4
# 5 "/usr/include/asm-generic/errno.h" 2 3 4
# 1 "/usr/include/i386-linux-gnu/asm/errno.h" 2 3 4
# 29 "/root/testvec/newlib-1.18.0/newlib/targ-include/sys/errno.h" 2
# 10 "/root/testvec/newlib-1.18.0/newlib/libc/include/errno.h" 2
# 37 "./vfieeefp.h" 2
# 84 "./vfieeefp.h"
struct ldieee
{
  unsigned manl:32;
  unsigned manh:32;
  unsigned exp:15;
  unsigned sign:1;
};
# 144 "./vfieeefp.h"
union double_union
{
  double d;
  __uint32_t i[2];
};
# 165 "./vfprintf.c" 2
# 329 "./vfprintf.c"
int __sprint_r (struct _reent *, FILE *, register struct __suio *);







static int
__sbprintf(struct _reent *rptr , register FILE *fp , const char *fmt , va_list ap)




{
 int ret;
 FILE fake;
 unsigned char buf[1024];


 fake._flags = fp->_flags & ~0x0002;
 fake._flags2 = fp->_flags2;
 fake._file = fp->_file;
 fake._cookie = fp->_cookie;
 fake._write = fp->_write;


 fake._bf._base = fake._p = buf;
 fake._bf._size = fake._w = sizeof (buf);
 fake._lbfsize = 0;

 do { if (__pthread_mutex_init != ((void *)0)) { pthread_mutexattr_t __attr; __pthread_mutexattr_init (&__attr); __pthread_mutexattr_settype (&__attr, PTHREAD_MUTEX_RECURSIVE_NP); __pthread_mutex_init (&(fake._lock).mutex, &__attr); __pthread_mutexattr_destroy (&__attr); } } while (0);;



 ret = _vfprintf_r (rptr, &fake, fmt, ap);
 if (ret >= 0 && _fflush_r (rptr, &fake))
  ret = (-1);
 if (fake._flags & 0x0040)
  fp->_flags |= 0x0040;


 ((__extension__ ({ __typeof (__pthread_mutex_destroy) *_fn = (__pthread_mutex_destroy); _fn != ((void *)0) ? (*_fn) (&((fake._lock).mutex)) : 0; })));;

 return (ret);
}




# 1 "/root/testvec/newlib-1.18.0/newlib/libc/include/locale.h" 1
# 24 "/root/testvec/newlib-1.18.0/newlib/libc/include/locale.h"


struct lconv
{
  char *decimal_point;
  char *thousands_sep;
  char *grouping;
  char *int_curr_symbol;
  char *currency_symbol;
  char *mon_decimal_point;
  char *mon_thousands_sep;
  char *mon_grouping;
  char *positive_sign;
  char *negative_sign;
  char int_frac_digits;
  char frac_digits;
  char p_cs_precedes;
  char p_sep_by_space;
  char n_cs_precedes;
  char n_sep_by_space;
  char p_sign_posn;
  char n_sign_posn;
  char int_n_cs_precedes;
  char int_n_sep_by_space;
  char int_n_sign_posn;
  char int_p_cs_precedes;
  char int_p_sep_by_space;
  char int_p_sign_posn;
};


char *setlocale (int category, const char *locale);
struct lconv *localeconv (void);


struct _reent;
char *_setlocale_r (struct _reent *, int category, const char *locale);
struct lconv *_localeconv_r (struct _reent *);


# 380 "./vfprintf.c" 2
# 398 "./vfprintf.c"
extern char *_ldtoa_r (struct _reent *, long double, int, int, int *, int *, char **)
                                     ;

extern int _ldcheck (long double *);
# 411 "./vfprintf.c"
static char *cvt(struct _reent *, long double, int, int, char *, int *,
                 int, int *, char *);

static int exponent(char *, int, int);
# 441 "./vfprintf.c"
typedef long long * quad_ptr_t;
typedef void * void_ptr_t;
typedef char * char_ptr_t;
typedef long * long_ptr_t;
typedef int * int_ptr_t;
typedef short * short_ptr_t;
# 455 "./vfprintf.c"
union arg_val
{
  int val_int;
  u_int val_u_int;
  long val_long;
  u_long val_u_long;
  float val_float;
  double val_double;
  long double val__LONG_DOUBLE;
  int_ptr_t val_int_ptr_t;
  short_ptr_t val_short_ptr_t;
  long_ptr_t val_long_ptr_t;
  char_ptr_t val_char_ptr_t;
  quad_ptr_t val_quad_ptr_t;
  void_ptr_t val_void_ptr_t;
  long long val_quad_t;
  unsigned long long val_u_quad_t;
  wint_t val_wint_t;
};

static union arg_val *
get_arg (struct _reent *data, int n, char *fmt, va_list *ap, int *numargs, union arg_val *args, int *arg_type, char **last_fmt)

                                                 ;
# 512 "./vfprintf.c"
int _vfprintf_r (struct _reent *, FILE *, const char *, va_list);


int
vfprintf(FILE * fp , const char *fmt0 , va_list ap)



{
  int result;
  result = _vfprintf_r ((__getreent()), fp, fmt0, ap);
  return result;
}


int
_vfprintf_r(struct _reent *data , FILE * fp , const char *fmt0 , va_list ap)




{
 register char *fmt;
 register int ch;
 register int n, m;
 register char *cp;
 register struct __siov *iovp;
 register int flags;
 char *fmt_anchor;

 int N;
 int arg_index;
 int numargs;
 char *saved_fmt;
 union arg_val args[32];
 int arg_type[32];
 int is_pos_arg;
 int old_is_pos_arg;

 int ret;
 int width;
 int prec;
 char sign;

 char *decimal_point = _localeconv_r (data)->decimal_point;
 size_t decp_len = strlen (decimal_point);
 char softsign;
 union { int i; long double fp; } _double_ = {0};

 int expt;
 int expsize = 0;
 int ndig = 0;
 char expstr[7];

 unsigned long long _uquad;
 enum { OCT, DEC, HEX } base;
 int dprec;
 int realsz;
 int size;
 char *xdigs = ((void *)0);

 struct __suio uio;
 struct __siov iov[8];
 char buf[40];
 char ox[2];

 wchar_t wc;
 mbstate_t state;

 char *malloc_buf = ((void *)0);







 static const char blanks[16] =
  {' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};
 static const char zeroes[16] =
  {'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'};


 memset (&state, '\0', sizeof (state));
# 676 "./vfprintf.c"
 do { if ((data) && !(data)->__sdidinit) __sinit (data); } while (0);
 flockfile(fp);

 do { if (!((fp)->_flags & 0x2000)) { (fp)->_flags |= 0x2000; if (-1 > 0) (fp)->_flags2 |= 0x2000; else (fp)->_flags2 &= ~0x2000; } } while (0);


 if (((((fp)->_flags & 0x0008) == 0 || (fp)->_bf._base == ((void *)0)) && __swsetup_r(data, fp))) {
  funlockfile(fp);
  return ((-1));
 }


 if ((fp->_flags & (0x0002|0x0008|0x0010)) == (0x0002|0x0008) &&
     fp->_file >= 0) {
  funlockfile(fp);
  return (__sbprintf (data, fp, fmt0, ap));
 }
# 707 "./vfprintf.c"
 fmt = (char *)fmt0;
 uio.uio_iov = iovp = iov;
 uio.uio_resid = 0;
 uio.uio_iovcnt = 0;
 ret = 0;

 arg_index = 0;
 saved_fmt = ((void *)0);
 arg_type[0] = -1;
 numargs = 0;
 is_pos_arg = 0;





 for (;;) {
         cp = fmt;

         while ((n = __mbtowc (data, &wc, fmt, __mb_cur_max,
          __locale_charset (), &state)) > 0) {
                    if (wc == '%')
                        break;
                    fmt += n;
  }




  if ((m = fmt - cp) != 0) {
   { iovp->iov_base = (cp); iovp->iov_len = (m); uio.uio_resid += (m); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
   ret += m;
  }

  if (n <= 0)
                    goto done;




  fmt_anchor = fmt;
  fmt++;

  flags = 0;
  dprec = 0;
  width = 0;
  prec = -1;
  sign = '\0';

  N = arg_index;
  is_pos_arg = 0;


rflag: ch = *fmt++;
reswitch: switch (ch) {

  case '\'':





    goto rflag;

  case ' ':





   if (!sign)
    sign = ' ';
   goto rflag;
  case '#':
   flags |= 0x001;
   goto rflag;
  case '*':


   n = N;
   old_is_pos_arg = is_pos_arg;
   is_pos_arg = 0;
   if (((unsigned)((*fmt) - '0') <= 9)) {
    char *old_fmt = fmt;

    n = 0;
    ch = *fmt++;
    do {
     n = 10 * n + ((ch) - '0');
     ch = *fmt++;
    } while (((unsigned)((ch) - '0') <= 9));

    if (ch == '$') {
     if (n <= 32) {
      n -= 1;
      is_pos_arg = 1;
     }
     else
      goto error;
    }
    else {
     fmt = old_fmt;
     goto rflag;
    }
   }
# 820 "./vfprintf.c"
   width = (is_pos_arg ? (n < numargs ? args[n].val_int : get_arg (data, n, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[n].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int))));

   is_pos_arg = old_is_pos_arg;

   if (width >= 0)
    goto rflag;
   width = -width;

  case '-':
   flags |= 0x004;
   goto rflag;
  case '+':
   sign = '+';
   goto rflag;
  case '.':
   if ((ch = *fmt++) == '*') {


    n = N;
    old_is_pos_arg = is_pos_arg;
    is_pos_arg = 0;
    if (((unsigned)((*fmt) - '0') <= 9)) {
     char *old_fmt = fmt;

     n = 0;
     ch = *fmt++;
     do {
      n = 10 * n + ((ch) - '0');
      ch = *fmt++;
     } while (((unsigned)((ch) - '0') <= 9));

     if (ch == '$') {
      if (n <= 32) {
       n -= 1;
       is_pos_arg = 1;
      }
      else
       goto error;
     }
     else {
      fmt = old_fmt;
      goto rflag;
     }
    }

    prec = (is_pos_arg ? (n < numargs ? args[n].val_int : get_arg (data, n, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[n].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int))));

    is_pos_arg = old_is_pos_arg;

    if (prec < 0)
     prec = -1;
    goto rflag;
   }
   n = 0;
   while (((unsigned)((ch) - '0') <= 9)) {
    n = 10 * n + ((ch) - '0');
    ch = *fmt++;
   }
   prec = n < 0 ? -1 : n;
   goto reswitch;
  case '0':





   flags |= 0x080;
   goto rflag;
  case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
   n = 0;
   do {
    n = 10 * n + ((ch) - '0');
    ch = *fmt++;
   } while (((unsigned)((ch) - '0') <= 9));

   if (ch == '$') {
    if (n <= 32) {
     N = n - 1;
     is_pos_arg = 1;
     goto rflag;
    }
    else
     goto error;
   }

   width = n;
   goto reswitch;

  case 'L':
   flags |= 0x008;
   goto rflag;

  case 'h':

   if (*fmt == 'h') {
    fmt++;
    flags |= 0x200;
   } else

    flags |= 0x040;
   goto rflag;
  case 'l':

   if (*fmt == 'l') {
    fmt++;
    flags |= 0x020;
   } else

    flags |= 0x010;
   goto rflag;
  case 'q':
   flags |= 0x020;
   goto rflag;

  case 'j':
    if (sizeof (intmax_t) == sizeof (long))
      flags |= 0x010;
    else
      flags |= 0x020;
    goto rflag;
  case 'z':
    if (sizeof (size_t) < sizeof (int))

      flags |= 0x040;
    else if (sizeof (size_t) == sizeof (int))
                          ;
    else if (sizeof (size_t) <= sizeof (long))
      flags |= 0x010;
    else




      flags |= 0x020;
    goto rflag;
  case 't':
    if (sizeof (ptrdiff_t) < sizeof (int))


      flags |= 0x040;
    else if (sizeof (ptrdiff_t) == sizeof (int))
                          ;
    else if (sizeof (ptrdiff_t) <= sizeof (long))
      flags |= 0x010;
    else




      flags |= 0x020;
    goto rflag;
  case 'C':

  case 'c':
   cp = buf;

   if (ch == 'C' || (flags & 0x010)) {
    mbstate_t ps;

    memset ((void *)&ps, '\0', sizeof (mbstate_t));
    if ((size = (int)_wcrtomb_r (data, cp,
            (wchar_t)(is_pos_arg ? (N < numargs ? args[N].val_wint_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_wint_t) : (arg_index++ < numargs ? args[N].val_wint_t : (numargs < 32 ? args[numargs++].val_wint_t = __builtin_va_arg(ap,wint_t) : __builtin_va_arg(ap,wint_t)))),
      &ps)) == -1) {
     fp->_flags |= 0x0040;
     goto error;
    }
   }
   else

   {
    *cp = (is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int))));
    size = 1;
   }
   sign = '\0';
   break;
  case 'D':
   flags |= 0x010;

  case 'd':
  case 'i':
   _uquad = (flags&0x020 ? (is_pos_arg ? (N < numargs ? args[N].val_quad_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_quad_t) : (arg_index++ < numargs ? args[N].val_quad_t : (numargs < 32 ? args[numargs++].val_quad_t = __builtin_va_arg(ap,long long) : __builtin_va_arg(ap,long long)))) : flags&0x010 ? (is_pos_arg ? (N < numargs ? args[N].val_long : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_long) : (arg_index++ < numargs ? args[N].val_long : (numargs < 32 ? args[numargs++].val_long = __builtin_va_arg(ap,long) : __builtin_va_arg(ap,long)))) : flags&0x040 ? (long)(short)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : flags&0x200 ? (long)(signed char)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : (long)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))));

   if ((long long)_uquad < 0)



   {

    _uquad = -_uquad;
    sign = '-';
   }
   base = DEC;
   goto number;


  case 'a':
  case 'A':
  case 'F':

  case 'e':
  case 'E':
  case 'f':
  case 'g':
  case 'G':
# 1061 "./vfprintf.c"
   if (flags & 0x008) {
    (_double_.fp) = (is_pos_arg ? (N < numargs ? args[N].val__LONG_DOUBLE : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val__LONG_DOUBLE) : (arg_index++ < numargs ? args[N].val__LONG_DOUBLE : (numargs < 32 ? args[numargs++].val__LONG_DOUBLE = __builtin_va_arg(ap,long double) : __builtin_va_arg(ap,long double))));
   } else {
    (_double_.fp) = (long double)(is_pos_arg ? (N < numargs ? args[N].val_double : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_double) : (arg_index++ < numargs ? args[N].val_double : (numargs < 32 ? args[numargs++].val_double = __builtin_va_arg(ap,double) : __builtin_va_arg(ap,double))));
   }


   expt = _ldcheck (&(_double_.fp));
   if (expt == 2) {
    if ((_double_.fp) < 0)
     sign = '-';
    if (ch <= 'G')
     cp = "INF";
    else
     cp = "inf";
    size = 3;
    flags &= ~0x080;
    break;
   }
   if (expt == 1) {
    if (ch <= 'G')
     cp = "NAN";
    else
     cp = "nan";
    size = 3;
    flags &= ~0x080;
    break;
   }



   if (ch == 'a' || ch == 'A') {
    ox[0] = '0';
    ox[1] = ch == 'a' ? 'x' : 'X';
    flags |= 0x002;
    if (prec >= 40)
      {
        if ((malloc_buf =
      (char *)_malloc_r (data, prec + 1))
     == ((void *)0))
          {
     fp->_flags |= 0x0040;
     goto error;
          }
        cp = malloc_buf;
      }
    else
      cp = buf;
   } else

   if (prec == -1) {
    prec = 6;
   } else if ((ch == 'g' || ch == 'G') && prec == 0) {
    prec = 1;
   }

   flags |= 0x100;

   cp = cvt (data, (_double_.fp), prec, flags, &softsign,
      &expt, ch, &ndig, cp);

   if (ch == 'g' || ch == 'G') {
    if (expt <= -4 || expt > prec)
     ch -= 2;
    else
     ch = 'g';
   }

   else if (ch == 'F')
    ch = 'f';

   if (ch <= 'e') {
    --expt;
    expsize = exponent (expstr, expt, ch);
    size = expsize + ndig;
    if (ndig > 1 || flags & 0x001)
     ++size;
   } else if (ch == 'f') {
    if (expt > 0) {
     size = expt;
     if (prec || flags & 0x001)
      size += prec + 1;
    } else
     size = (prec || flags & 0x001)
        ? prec + 2
        : 1;
   } else if (expt >= ndig) {
    size = expt;
    if (flags & 0x001)
     ++size;
   } else
    size = ndig + (expt > 0 ?
     1 : 2 - expt);

   if (softsign)
    sign = '-';
   break;

  case 'n':

   if (flags & 0x020)
    *(is_pos_arg ? (N < numargs ? args[N].val_quad_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_quad_ptr_t) : (arg_index++ < numargs ? args[N].val_quad_ptr_t : (numargs < 32 ? args[numargs++].val_quad_ptr_t = __builtin_va_arg(ap,quad_ptr_t) : __builtin_va_arg(ap,quad_ptr_t)))) = ret;
   else

   if (flags & 0x010)
    *(is_pos_arg ? (N < numargs ? args[N].val_long_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_long_ptr_t) : (arg_index++ < numargs ? args[N].val_long_ptr_t : (numargs < 32 ? args[numargs++].val_long_ptr_t = __builtin_va_arg(ap,long_ptr_t) : __builtin_va_arg(ap,long_ptr_t)))) = ret;
   else if (flags & 0x040)
    *(is_pos_arg ? (N < numargs ? args[N].val_short_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_short_ptr_t) : (arg_index++ < numargs ? args[N].val_short_ptr_t : (numargs < 32 ? args[numargs++].val_short_ptr_t = __builtin_va_arg(ap,short_ptr_t) : __builtin_va_arg(ap,short_ptr_t)))) = ret;

   else if (flags & 0x200)
    *(is_pos_arg ? (N < numargs ? args[N].val_char_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_char_ptr_t) : (arg_index++ < numargs ? args[N].val_char_ptr_t : (numargs < 32 ? args[numargs++].val_char_ptr_t = __builtin_va_arg(ap,char_ptr_t) : __builtin_va_arg(ap,char_ptr_t)))) = ret;

   else
    *(is_pos_arg ? (N < numargs ? args[N].val_int_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int_ptr_t) : (arg_index++ < numargs ? args[N].val_int_ptr_t : (numargs < 32 ? args[numargs++].val_int_ptr_t = __builtin_va_arg(ap,int_ptr_t) : __builtin_va_arg(ap,int_ptr_t)))) = ret;
   continue;
  case 'O':
   flags |= 0x010;

  case 'o':
   _uquad = (flags&0x020 ? (is_pos_arg ? (N < numargs ? args[N].val_u_quad_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_quad_t) : (arg_index++ < numargs ? args[N].val_u_quad_t : (numargs < 32 ? args[numargs++].val_u_quad_t = __builtin_va_arg(ap,unsigned long long) : __builtin_va_arg(ap,unsigned long long)))) : flags&0x010 ? (is_pos_arg ? (N < numargs ? args[N].val_u_long : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_long) : (arg_index++ < numargs ? args[N].val_u_long : (numargs < 32 ? args[numargs++].val_u_long = __builtin_va_arg(ap,u_long) : __builtin_va_arg(ap,u_long)))) : flags&0x040 ? (u_long)(u_short)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : flags&0x200 ? (u_long)(unsigned char)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : (u_long)(is_pos_arg ? (N < numargs ? args[N].val_u_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_int) : (arg_index++ < numargs ? args[N].val_u_int : (numargs < 32 ? args[numargs++].val_u_int = __builtin_va_arg(ap,u_int) : __builtin_va_arg(ap,u_int)))));
   base = OCT;
   goto nosign;
  case 'p':
# 1192 "./vfprintf.c"
   _uquad = (uintptr_t) (is_pos_arg ? (N < numargs ? args[N].val_void_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_void_ptr_t) : (arg_index++ < numargs ? args[N].val_void_ptr_t : (numargs < 32 ? args[numargs++].val_void_ptr_t = __builtin_va_arg(ap,void_ptr_t) : __builtin_va_arg(ap,void_ptr_t))));
   base = HEX;
   xdigs = "0123456789abcdef";
   flags |= 0x002;
   ox[0] = '0';
   ox[1] = ch = 'x';
   goto nosign;
  case 's':

  case 'S':

   sign = '\0';
   cp = (is_pos_arg ? (N < numargs ? args[N].val_char_ptr_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_char_ptr_t) : (arg_index++ < numargs ? args[N].val_char_ptr_t : (numargs < 32 ? args[numargs++].val_char_ptr_t = __builtin_va_arg(ap,char_ptr_t) : __builtin_va_arg(ap,char_ptr_t))));





   if (cp == ((void *)0)) {
    cp = "(null)";
    size = ((unsigned) prec > 6U) ? 6 : prec;
   }
   else


   if (ch == 'S' || (flags & 0x010)) {
    mbstate_t ps;
    const wchar_t *wcp;

    wcp = (const wchar_t *)cp;
    size = m = 0;
    memset ((void *)&ps, '\0', sizeof (mbstate_t));




    if (prec >= 0) {
     while (1) {
      if (wcp[m] == L'\0')
       break;
      if ((n = (int)_wcrtomb_r (data,
           buf, wcp[m], &ps)) == -1) {
       fp->_flags |= 0x0040;
       goto error;
      }
      if (n + size > prec)
       break;
      m += 1;
      size += n;
      if (size == prec)
       break;
     }
    }
    else {
     if ((size = (int)_wcsrtombs_r (data,
         ((void *)0), &wcp, 0, &ps)) == -1) {
      fp->_flags |= 0x0040;
      goto error;
     }
     wcp = (const wchar_t *)cp;
    }

    if (size == 0)
     break;

    if (size >= 40) {
     if ((malloc_buf =
          (char *)_malloc_r (data, size + 1))
         == ((void *)0)) {
      fp->_flags |= 0x0040;
      goto error;
     }
     cp = malloc_buf;
    } else
     cp = buf;


    memset ((void *)&ps, '\0', sizeof (mbstate_t));
    if (_wcsrtombs_r (data, cp, &wcp, size, &ps)
        != size) {
     fp->_flags |= 0x0040;
     goto error;
    }
    cp[size] = '\0';
   }
   else

   if (prec >= 0) {





    char *p = memchr (cp, 0, prec);

    if (p != ((void *)0)) {
     size = p - cp;
     if (size > prec)
      size = prec;
    } else
     size = prec;
   } else
    size = strlen (cp);

   break;
  case 'U':
   flags |= 0x010;

  case 'u':
   _uquad = (flags&0x020 ? (is_pos_arg ? (N < numargs ? args[N].val_u_quad_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_quad_t) : (arg_index++ < numargs ? args[N].val_u_quad_t : (numargs < 32 ? args[numargs++].val_u_quad_t = __builtin_va_arg(ap,unsigned long long) : __builtin_va_arg(ap,unsigned long long)))) : flags&0x010 ? (is_pos_arg ? (N < numargs ? args[N].val_u_long : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_long) : (arg_index++ < numargs ? args[N].val_u_long : (numargs < 32 ? args[numargs++].val_u_long = __builtin_va_arg(ap,u_long) : __builtin_va_arg(ap,u_long)))) : flags&0x040 ? (u_long)(u_short)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : flags&0x200 ? (u_long)(unsigned char)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : (u_long)(is_pos_arg ? (N < numargs ? args[N].val_u_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_int) : (arg_index++ < numargs ? args[N].val_u_int : (numargs < 32 ? args[numargs++].val_u_int = __builtin_va_arg(ap,u_int) : __builtin_va_arg(ap,u_int)))));
   base = DEC;
   goto nosign;
  case 'X':
   xdigs = "0123456789ABCDEF";
   goto hex;
  case 'x':
   xdigs = "0123456789abcdef";
hex: _uquad = (flags&0x020 ? (is_pos_arg ? (N < numargs ? args[N].val_u_quad_t : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_quad_t) : (arg_index++ < numargs ? args[N].val_u_quad_t : (numargs < 32 ? args[numargs++].val_u_quad_t = __builtin_va_arg(ap,unsigned long long) : __builtin_va_arg(ap,unsigned long long)))) : flags&0x010 ? (is_pos_arg ? (N < numargs ? args[N].val_u_long : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_long) : (arg_index++ < numargs ? args[N].val_u_long : (numargs < 32 ? args[numargs++].val_u_long = __builtin_va_arg(ap,u_long) : __builtin_va_arg(ap,u_long)))) : flags&0x040 ? (u_long)(u_short)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : flags&0x200 ? (u_long)(unsigned char)(is_pos_arg ? (N < numargs ? args[N].val_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_int) : (arg_index++ < numargs ? args[N].val_int : (numargs < 32 ? args[numargs++].val_int = __builtin_va_arg(ap,int) : __builtin_va_arg(ap,int)))) : (u_long)(is_pos_arg ? (N < numargs ? args[N].val_u_int : get_arg (data, N, fmt_anchor, &ap, &numargs, args, arg_type, &saved_fmt)->val_u_int) : (arg_index++ < numargs ? args[N].val_u_int : (numargs < 32 ? args[numargs++].val_u_int = __builtin_va_arg(ap,u_int) : __builtin_va_arg(ap,u_int)))));
   base = HEX;

   if (flags & 0x001 && _uquad != 0) {
    ox[0] = '0';
    ox[1] = ch;
    flags |= 0x002;
   }


nosign: sign = '\0';





number: if ((dprec = prec) >= 0)
    flags &= ~0x080;






   cp = buf + 40;
   if (_uquad != 0 || prec != 0) {





    switch (base) {
    case OCT:
     do {
      *--cp = ((_uquad & 7) + '0');
      _uquad >>= 3;
     } while (_uquad);

     if (flags & 0x001 && *cp != '0')
      *--cp = '0';
     break;

    case DEC:

     while (_uquad >= 10) {
      *--cp = ((_uquad % 10) + '0');
      _uquad /= 10;
     }
     *--cp = ((_uquad) + '0');
     break;

    case HEX:
     do {
      *--cp = xdigs[_uquad & 15];
      _uquad >>= 4;
     } while (_uquad);
     break;

    default:
     cp = "bug in vfprintf: bad base";
     size = strlen (cp);
     goto skipsize;
    }
   }
# 1382 "./vfprintf.c"
                       else if (base == OCT && (flags & 0x001))
                         *--cp = '0';

   size = buf + 40 - cp;
  skipsize:
   break;
  default:
   if (ch == '\0')
    goto done;

   cp = buf;
   *cp = ch;
   size = 1;
   sign = '\0';
   break;
  }
# 1414 "./vfprintf.c"
  realsz = dprec > size ? dprec : size;
  if (sign)
   realsz++;
  if (flags & 0x002)
   realsz+= 2;


  if ((flags & (0x004|0x080)) == 0)
   { if ((n = (width - realsz)) > 0) { while (n > 16) { { iovp->iov_base = (blanks); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (blanks); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };


  if (sign)
   { iovp->iov_base = (&sign); iovp->iov_len = (1); uio.uio_resid += (1); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
  if (flags & 0x002)
   { iovp->iov_base = (ox); iovp->iov_len = (2); uio.uio_resid += (2); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };


  if ((flags & (0x004|0x080)) == 0x080)
   { if ((n = (width - realsz)) > 0) { while (n > 16) { { iovp->iov_base = (zeroes); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (zeroes); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };


  { if ((n = (dprec - size)) > 0) { while (n > 16) { { iovp->iov_base = (zeroes); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (zeroes); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };



  if ((flags & 0x100) == 0) {
   { iovp->iov_base = (cp); iovp->iov_len = (size); uio.uio_resid += (size); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
  } else {
   if (ch >= 'f') {
    if ((_double_.fp) == 0) {

     { iovp->iov_base = ("0"); iovp->iov_len = (1); uio.uio_resid += (1); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     if (expt < ndig || flags & 0x001) {
      { iovp->iov_base = (decimal_point); iovp->iov_len = (decp_len); uio.uio_resid += (decp_len); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
      { if ((n = (ndig - 1)) > 0) { while (n > 16) { { iovp->iov_base = (zeroes); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (zeroes); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };
     }
    } else if (expt <= 0) {
     { iovp->iov_base = ("0"); iovp->iov_len = (1); uio.uio_resid += (1); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     if (expt || ndig || flags & 0x001) {
      { iovp->iov_base = (decimal_point); iovp->iov_len = (decp_len); uio.uio_resid += (decp_len); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
      { if ((n = (-expt)) > 0) { while (n > 16) { { iovp->iov_base = (zeroes); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (zeroes); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };
      { iovp->iov_base = (cp); iovp->iov_len = (ndig); uio.uio_resid += (ndig); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     }
    } else if (expt >= ndig) {
     { iovp->iov_base = (cp); iovp->iov_len = (ndig); uio.uio_resid += (ndig); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     { if ((n = (expt - ndig)) > 0) { while (n > 16) { { iovp->iov_base = (zeroes); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (zeroes); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };
     if (flags & 0x001)
      { iovp->iov_base = (decimal_point); iovp->iov_len = (decp_len); uio.uio_resid += (decp_len); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
    } else {
     { iovp->iov_base = (cp); iovp->iov_len = (expt); uio.uio_resid += (expt); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     cp += expt;
     { iovp->iov_base = (decimal_point); iovp->iov_len = (decp_len); uio.uio_resid += (decp_len); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     { iovp->iov_base = (cp); iovp->iov_len = (ndig - expt); uio.uio_resid += (ndig - expt); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
    }
   } else {
    if (ndig > 1 || flags & 0x001) {
     { iovp->iov_base = (cp); iovp->iov_len = (1); uio.uio_resid += (1); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     cp++;
     { iovp->iov_base = (decimal_point); iovp->iov_len = (decp_len); uio.uio_resid += (decp_len); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     if ((_double_.fp)) {
      { iovp->iov_base = (cp); iovp->iov_len = (ndig - 1); uio.uio_resid += (ndig - 1); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
     } else

      { if ((n = (ndig - 1)) > 0) { while (n > 16) { { iovp->iov_base = (zeroes); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (zeroes); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };
    } else
     { iovp->iov_base = (cp); iovp->iov_len = (1); uio.uio_resid += (1); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
    { iovp->iov_base = (expstr); iovp->iov_len = (expsize); uio.uio_resid += (expsize); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } };
   }
  }




  if (flags & 0x004)
   { if ((n = (width - realsz)) > 0) { while (n > 16) { { iovp->iov_base = (blanks); iovp->iov_len = (16); uio.uio_resid += (16); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; n -= 16; } { iovp->iov_base = (blanks); iovp->iov_len = (n); uio.uio_resid += (n); iovp++; if (++uio.uio_iovcnt >= 8) { if (__sprint_r(data, fp, &uio)) goto error; iovp = iov; } }; } };


  ret += width > realsz ? width : realsz;

  { if (uio.uio_resid && __sprint_r(data, fp, &uio)) goto error; uio.uio_iovcnt = 0; iovp = iov; };

                if (malloc_buf != ((void *)0)) {
   _free_r (data, malloc_buf);
   malloc_buf = ((void *)0);
  }
 }
done:
 { if (uio.uio_resid && __sprint_r(data, fp, &uio)) goto error; uio.uio_iovcnt = 0; iovp = iov; };
error:
 if (malloc_buf != ((void *)0))
  _free_r (data, malloc_buf);

 funlockfile(fp);

 return ((((fp)->_flags & 0x0040) != 0) ? (-1) : ret);

}
# 1521 "./vfprintf.c"
static char *
cvt(struct _reent *data, long double value, int ndigits, int flags,
    char *sign, int *decpt, int ch, int *length, char *buf)
{
 int mode, dsgn;
 char *digits, *bp, *rve;
# 1537 "./vfprintf.c"
 union
 {
   struct ldieee ieee;
   long double val;
 } ld;

 ld.val = value;
 if (ld.ieee.sign) {
  value = -value;
  *sign = '-';
 } else
  *sign = '\000';



 if (ch == 'a' || ch == 'A') {




  value = ((long double) frexp ((double)value, decpt)) / 8;
  if (!value)
   *decpt = 1;
  digits = ch == 'a' ? "0123456789abcdef" : "0123456789ABCDEF";
  bp = buf;
  do {
   value *= 16;
   mode = (int) value;
   value -= mode;
   *bp++ = digits[mode];
  } while (ndigits-- && value);
  if (value > 0.5 || (value == 0.5 && mode & 1)) {

   rve = bp;
   while (*--rve == digits[0xf]) {
    *rve = '0';
   }
   *rve = *rve == '9' ? digits[0xa] : *rve + 1;
  } else {
   while (ndigits-- >= 0) {
    *bp++ = '0';
   }
  }
  *length = bp - buf;
  return buf;
 }

 if (ch == 'f' || ch == 'F') {
  mode = 3;
 } else {




  if (ch == 'e' || ch == 'E') {
   ndigits++;
  }
  mode = 2;
 }

 digits = _ldtoa_r (data, value, mode, ndigits, decpt, &dsgn, &rve);

 if ((ch != 'g' && ch != 'G') || flags & 0x001) {
  bp = digits + ndigits;
  if (ch == 'f' || ch == 'F') {
   if (*digits == '0' && value)
    *decpt = -ndigits + 1;
   bp += *decpt;
  }
  if (value == 0)
   rve = bp;
  while (rve < bp)
   *rve++ = '0';
 }
 *length = rve - digits;
 return (digits);
}

static int
exponent(char *p0, int exp, int fmtch)
{
 register char *p, *t;
 char expbuf[7];

 int isa = fmtch == 'a' || fmtch == 'A';




 p = p0;
 *p++ = isa ? 'p' - 'a' + fmtch : fmtch;
 if (exp < 0) {
  exp = -exp;
  *p++ = '-';
 }
 else
  *p++ = '+';
 t = expbuf + 7;
 if (exp > 9) {
  do {
   *--t = ((exp % 10) + '0');
  } while ((exp /= 10) > 9);
  *--t = ((exp) + '0');
  for (; t < expbuf + 7; *p++ = *t++);
 }
 else {
  if (!isa)
   *p++ = '0';
  *p++ = ((exp) + '0');
 }
 return (p - p0);
}
# 1758 "./vfprintf.c"
static union arg_val *
get_arg(struct _reent *data , int n , char *fmt , va_list *ap , int *numargs_p , union arg_val *args , int *arg_type , char **last_fmt)
# 1768 "./vfprintf.c"
{
  int ch;
  int number, flags;
  int spec_type;
  int numargs = *numargs_p;
  __CH_CLASS chtype;
  __STATE state, next_state;
  __ACTION action;
  int pos, last_arg;
  int max_pos_arg = n;

  enum types { INT, LONG_INT, QUAD_INT, CHAR_PTR, DOUBLE, LONG_DOUBLE, WIDE_CHAR };

  wchar_t wc;
  mbstate_t wc_state;
  int nbytes;



  if (*last_fmt != ((void *)0))
    fmt = *last_fmt;


  memset (&wc_state, '\0', sizeof (wc_state));




  while (*fmt && n >= numargs)
    {

      while ((nbytes = __mbtowc (data, &wc, fmt, __mb_cur_max,
     __locale_charset (), &wc_state)) > 0)
 {
   fmt += nbytes;
   if (wc == '%')
     break;
 }

      if (nbytes <= 0)
 break;







      state = START;
      flags = 0;
      pos = -1;
      number = 0;
      spec_type = INT;




      while (state != DONE)
 {
   ch = *fmt++;
   chtype = __chclass[ch];
   next_state = __state_table[state][chtype];
   action = __action_table[state][chtype];
   state = next_state;

   switch (action)
     {
     case GETMOD:
       switch (ch)
  {
  case 'h':

    break;
  case 'L':
    flags |= 0x008;
    break;
  case 'q':
    flags |= 0x020;
    break;

  case 'j':
    if (sizeof (intmax_t) == sizeof (long))
      flags |= 0x010;
    else
      flags |= 0x020;
    break;
  case 'z':
    if (sizeof (size_t) <= sizeof (int))
                          ;
    else if (sizeof (size_t) <= sizeof (long))
      flags |= 0x010;
    else




      flags |= 0x020;
    break;
  case 't':
    if (sizeof (ptrdiff_t) <= sizeof (int))
                          ;
    else if (sizeof (ptrdiff_t) <= sizeof (long))
      flags |= 0x010;
    else




      flags |= 0x020;
    break;

  case 'l':
  default:

    if (*fmt == 'l')
      {
        flags |= 0x020;
        ++fmt;
      }
    else

      flags |= 0x010;
    break;
  }
       break;
     case GETARG:
       {
  numargs &= (32 - 1);

  switch (ch)
    {
    case 'd':
    case 'i':
    case 'o':
    case 'x':
    case 'X':
    case 'u':
      if (flags & 0x010)
        spec_type = LONG_INT;

      else if (flags & 0x020)
        spec_type = QUAD_INT;

      else
        spec_type = INT;
      break;
    case 'D':
    case 'U':
    case 'O':
      spec_type = LONG_INT;
      break;

    case 'a':
    case 'A':
    case 'F':

    case 'f':
    case 'g':
    case 'G':
    case 'E':
    case 'e':

      if (flags & 0x008)
        spec_type = LONG_DOUBLE;
      else

        spec_type = DOUBLE;
      break;
    case 's':

    case 'S':

    case 'p':
    case 'n':
      spec_type = CHAR_PTR;
      break;
    case 'c':

      if (flags & 0x010)
        spec_type = WIDE_CHAR;
      else

        spec_type = INT;
      break;

    case 'C':
      spec_type = WIDE_CHAR;
      break;

    }



  if (pos != -1)
    arg_type[pos] = spec_type;
  else
    {
      switch (spec_type)
        {
        case LONG_INT:
   args[numargs++].val_long = __builtin_va_arg(*ap,long);
   break;
        case QUAD_INT:
   args[numargs++].val_quad_t = __builtin_va_arg(*ap,long long);
   break;
        case WIDE_CHAR:
   args[numargs++].val_wint_t = __builtin_va_arg(*ap,wint_t);
   break;
        case INT:
   args[numargs++].val_int = __builtin_va_arg(*ap,int);
   break;
        case CHAR_PTR:
   args[numargs++].val_char_ptr_t = __builtin_va_arg(*ap,char *);
   break;
        case DOUBLE:
   args[numargs++].val_double = __builtin_va_arg(*ap,double);
   break;
        case LONG_DOUBLE:
   args[numargs++].val__LONG_DOUBLE = __builtin_va_arg(*ap,long double);
   break;
        }
    }
       }
       break;
     case GETPOS:
       if (arg_type[0] == -1)
  memset (arg_type, 0, sizeof (int) * 32);
       pos = number - 1;
       max_pos_arg = (max_pos_arg > pos ? max_pos_arg : pos);
       break;
     case PWPOS:
       if (arg_type[0] == -1)
  memset (arg_type, 0, sizeof (int) * 32);
       number -= 1;
       arg_type[number] = INT;
       max_pos_arg = (max_pos_arg > number ? max_pos_arg : number);
       break;
     case GETPWB:
       --fmt;

     case GETPW:
       args[numargs++].val_int = __builtin_va_arg(*ap,int);
       break;
     case NUMBER:
       number = (ch - '0');
       while ((ch = *fmt) != '\0' && ((unsigned)((ch) - '0') <= 9))
  {
    number = number * 10 + (ch - '0');
    ++fmt;
  }
       break;
     case SKIPNUM:
       while ((ch = *fmt) != '\0' && ((unsigned)((ch) - '0') <= 9))
  ++fmt;
       break;
     case NOOP:
     default:
       break;
     }
 }
    }



  if (*fmt == '\0')
    last_arg = max_pos_arg;
  else
    last_arg = n;

  while (numargs <= last_arg)
    {
      switch (arg_type[numargs])
 {
 case LONG_INT:
   args[numargs++].val_long = __builtin_va_arg(*ap,long);
   break;
 case QUAD_INT:
   args[numargs++].val_quad_t = __builtin_va_arg(*ap,long long);
   break;
 case CHAR_PTR:
   args[numargs++].val_char_ptr_t = __builtin_va_arg(*ap,char *);
   break;
 case DOUBLE:
   args[numargs++].val_double = __builtin_va_arg(*ap,double);
   break;
 case LONG_DOUBLE:
   args[numargs++].val__LONG_DOUBLE = __builtin_va_arg(*ap,long double);
   break;
 case WIDE_CHAR:
   args[numargs++].val_wint_t = __builtin_va_arg(*ap,wint_t);
   break;
 case INT:
 default:
   args[numargs++].val_int = __builtin_va_arg(*ap,int);
   break;
 }
    }



  *numargs_p = numargs;
  *last_fmt = fmt;
  return &args[n];
}
