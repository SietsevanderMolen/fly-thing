pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package asm_generic_posix_types_h is

  -- * This file is generally used by user-level software, so you need to
  -- * be a little careful about namespace pollution etc.
  -- *
  -- * First the types that are often defined in different ways across
  -- * architectures, so that you can override them.
  --  

   subtype uu_kernel_long_t is long;  -- /usr/include/asm-generic/posix_types.h:14

   subtype uu_kernel_ulong_t is unsigned_long;  -- /usr/include/asm-generic/posix_types.h:15

   subtype uu_kernel_ino_t is uu_kernel_ulong_t;  -- /usr/include/asm-generic/posix_types.h:19

   subtype uu_kernel_mode_t is unsigned;  -- /usr/include/asm-generic/posix_types.h:23

   subtype uu_kernel_pid_t is int;  -- /usr/include/asm-generic/posix_types.h:27

   subtype uu_kernel_ipc_pid_t is int;  -- /usr/include/asm-generic/posix_types.h:31

   subtype uu_kernel_uid_t is unsigned;  -- /usr/include/asm-generic/posix_types.h:35

   subtype uu_kernel_gid_t is unsigned;  -- /usr/include/asm-generic/posix_types.h:36

   subtype uu_kernel_suseconds_t is uu_kernel_long_t;  -- /usr/include/asm-generic/posix_types.h:40

   subtype uu_kernel_daddr_t is int;  -- /usr/include/asm-generic/posix_types.h:44

   subtype uu_kernel_uid32_t is unsigned;  -- /usr/include/asm-generic/posix_types.h:48

   subtype uu_kernel_gid32_t is unsigned;  -- /usr/include/asm-generic/posix_types.h:49

  -- * Most 32 bit architectures use "unsigned int" size_t,
  -- * and all 64 bit architectures use "unsigned long" size_t.
  --  

   subtype uu_kernel_size_t is uu_kernel_ulong_t;  -- /usr/include/asm-generic/posix_types.h:71

   subtype uu_kernel_ssize_t is uu_kernel_long_t;  -- /usr/include/asm-generic/posix_types.h:72

   subtype uu_kernel_ptrdiff_t is uu_kernel_long_t;  -- /usr/include/asm-generic/posix_types.h:73

   type uu_kernel_fsid_t_val_array is array (0 .. 1) of aliased int;
   type uu_kernel_fsid_t is record
      val : aliased uu_kernel_fsid_t_val_array;  -- /usr/include/asm-generic/posix_types.h:79
   end record;
   pragma Convention (C_Pass_By_Copy, uu_kernel_fsid_t);  -- /usr/include/asm-generic/posix_types.h:80

   --  skipped anonymous struct anon_1

  -- * anything below here should be completely generic
  --  

   subtype uu_kernel_off_t is uu_kernel_long_t;  -- /usr/include/asm-generic/posix_types.h:86

   subtype uu_kernel_loff_t is Long_Long_Integer;  -- /usr/include/asm-generic/posix_types.h:87

   subtype uu_kernel_time_t is uu_kernel_long_t;  -- /usr/include/asm-generic/posix_types.h:88

   subtype uu_kernel_clock_t is uu_kernel_long_t;  -- /usr/include/asm-generic/posix_types.h:89

   subtype uu_kernel_timer_t is int;  -- /usr/include/asm-generic/posix_types.h:90

   subtype uu_kernel_clockid_t is int;  -- /usr/include/asm-generic/posix_types.h:91

   type uu_kernel_caddr_t is new Interfaces.C.Strings.chars_ptr;  -- /usr/include/asm-generic/posix_types.h:92

   subtype uu_kernel_uid16_t is unsigned_short;  -- /usr/include/asm-generic/posix_types.h:93

   subtype uu_kernel_gid16_t is unsigned_short;  -- /usr/include/asm-generic/posix_types.h:94

end asm_generic_posix_types_h;
