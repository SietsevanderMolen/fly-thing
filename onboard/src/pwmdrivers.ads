package PWMDrivers is
   type PWMDriver is private;

   function Create (Address : Integer) return PWMDriver;
   procedure Reset (P : PWMDriver);
   procedure SetPWMFreq (Frequency : Float);
   procedure SetPWM (Number : Integer;
                     On : Integer;
                     Off : Integer);
   procedure SetPin (Number : Integer;
                     Value : Integer;
                     Invert : Boolean := False);
private
   type PWMDriver is
      record
         Address : Integer;
      end record;

   function Create (Address : Integer) return PWMDriver
      is (Address => Address);

   function Read (Address : Integer) return Integer;
   procedure Write (Address : Integer; Value : Integer);
end PWMDrivers;
