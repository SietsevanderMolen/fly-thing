pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package bits_ioctl_types_h is

   --  unsupported macro: NCC 8
   --  unsupported macro: TIOCM_LE 0x001
   --  unsupported macro: TIOCM_DTR 0x002
   --  unsupported macro: TIOCM_RTS 0x004
   --  unsupported macro: TIOCM_ST 0x008
   --  unsupported macro: TIOCM_SR 0x010
   --  unsupported macro: TIOCM_CTS 0x020
   --  unsupported macro: TIOCM_CAR 0x040
   --  unsupported macro: TIOCM_RNG 0x080
   --  unsupported macro: TIOCM_DSR 0x100
   --  unsupported macro: TIOCM_CD TIOCM_CAR
   --  unsupported macro: TIOCM_RI TIOCM_RNG
   --  unsupported macro: N_TTY 0
   --  unsupported macro: N_SLIP 1
   --  unsupported macro: N_MOUSE 2
   --  unsupported macro: N_PPP 3
   --  unsupported macro: N_STRIP 4
   --  unsupported macro: N_AX25 5
   --  unsupported macro: N_X25 6
   --  unsupported macro: N_6PACK 7
   --  unsupported macro: N_MASC 8
   --  unsupported macro: N_R3964 9
   --  unsupported macro: N_PROFIBUS_FDL 10
   --  unsupported macro: N_IRDA 11
   --  unsupported macro: N_SMSBLOCK 12
   --  unsupported macro: N_HDLC 13
   --  unsupported macro: N_SYNC_PPP 14
   --  unsupported macro: N_HCI 15
  -- Structure types for pre-termios terminal ioctls.  Linux version.
  --   Copyright (C) 1996-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <http://www.gnu.org/licenses/>.   

  -- Get definition of constants for use with `ioctl'.   
   type winsize is record
      ws_row : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:29
      ws_col : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:30
      ws_xpixel : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:31
      ws_ypixel : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:32
   end record;
   pragma Convention (C_Pass_By_Copy, winsize);  -- /usr/include/bits/ioctl-types.h:27

  -- input mode flags  
   type anon969_anon971_array is array (0 .. 7) of aliased unsigned_char;
   type termio is record
      c_iflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:38
      c_oflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:39
      c_cflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:40
      c_lflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:41
      c_line : aliased unsigned_char;  -- /usr/include/bits/ioctl-types.h:42
      c_cc : aliased anon969_anon971_array;  -- /usr/include/bits/ioctl-types.h:43
   end record;
   pragma Convention (C_Pass_By_Copy, termio);  -- /usr/include/bits/ioctl-types.h:36

  -- output mode flags  
  -- control mode flags  
  -- local mode flags  
  -- line discipline  
  -- control characters  
  -- modem lines  
  -- ioctl (fd, TIOCSERGETLSR, &result) where result may be as below  
  -- line disciplines  
end bits_ioctl_types_h;
