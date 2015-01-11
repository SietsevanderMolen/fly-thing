package body PWMDrivers is
   procedure Reset (P : PWMDriver) is
   begin
      null;
   end Reset;

   procedure SetPWMFreq (Frequency : Float) is
   begin
      null;
   end SetPWMFreq;

   procedure SetPWM (Number : Integer;
                     On : Integer;
                     Off : Integer) is
   begin
      null;
   end SetPWM;

   procedure SetPin (Number : Integer;
                     Value : Integer;
                     Invert : Boolean := False) is
   begin
      null;
   end SetPin;

   function Read (Address : Integer) return Integer is
      Return_Value : constant Integer := Address;
   begin
      return Return_Value;
   end Read;

   procedure Write (Address : Integer; Value : Integer) is
   begin
      null;
   end Write;
end PWMDrivers;
