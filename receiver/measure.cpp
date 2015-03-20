#include <PinChangeInt.h>
#include <Servo.h>

// Channel in pins
#define MOTOR_1_IN_PIN 9
#define MOTOR_2_IN_PIN 10
#define MOTOR_3_IN_PIN 11
#define MOTOR_4_IN_PIN 12

// Channel out pins
#define MOTOR_1_OUT_PIN 5
#define MOTOR_2_OUT_PIN 6
#define MOTOR_3_OUT_PIN 7
#define MOTOR_4_OUT_PIN 8

Servo escMotor1;
Servo escMotor2;
Servo escMotor3;
Servo escMotor4;

// New signal indicators
#define MOTOR_1_FLAG 1
#define MOTOR_2_FLAG 2
#define MOTOR_3_FLAG 4
#define MOTOR_4_FLAG 8

// Holds the update flags defined above
volatile uint8_t bUpdateFlagsShared;

// Shared variables, updated by the ISR and read by the loop.  In the loop the
// values are copied immediately so that the ISR can keep ownership. To access
// these in loop the interrupts are turned off with noInterrupts temporarily
volatile uint16_t unMotor1InShared;
volatile uint16_t unMotor2InShared;
volatile uint16_t unMotor3InShared;
volatile uint16_t unMotor4InShared;

// Used to record the rising edge of a pulse in the calcInput functions. Don't
// need to be volatile as they are only used in the ISR.
uint32_t ulMotor1Start;
uint32_t ulMotor2Start;
uint32_t ulMotor3Start;
uint32_t ulMotor4Start;

void calcMotor1()
{
  // if the pin is high, its a rising - record its value
  if(digitalRead(MOTOR_1_IN_PIN) == HIGH)
  { 
    ulMotor1Start = micros();
  }
  else
  {
    // else it's a falling edge -get the time and subtract the time of the
    // rising edge to get pulse duration
    unMotor1InShared = (uint16_t)(micros() - ulMotor1Start);
    // set the appropriate flag to indicate that a signal has been received
    bUpdateFlagsShared |= MOTOR_1_FLAG; } }

void calcMotor2()
{
  if(digitalRead(MOTOR_2_IN_PIN) == HIGH)
  { 
    ulMotor2Start = micros();
  }
  else
  {
    unMotor2InShared = (uint16_t)(micros() - ulMotor2Start);
    bUpdateFlagsShared |= MOTOR_2_FLAG;
  }
}

void calcMotor3()
{
  if(digitalRead(MOTOR_3_IN_PIN) == HIGH)
  { 
    ulMotor3Start = micros();
  }
  else
  {
    unMotor3InShared = (uint16_t)(micros() - ulMotor3Start);
    bUpdateFlagsShared |= MOTOR_3_FLAG;
  }
}

void calcMotor4()
{
  if(digitalRead(MOTOR_4_IN_PIN) == HIGH)
  { 
    ulMotor4Start = micros();
  }
  else
  {
    unMotor4InShared = (uint16_t)(micros() - ulMotor4Start);
    bUpdateFlagsShared |= MOTOR_4_FLAG;
  }
}

void setup()
{
  Serial.begin(9600);
  
  Serial.println("multiChannels");

  escMotor1.attach(MOTOR_1_OUT_PIN);
  escMotor2.attach(MOTOR_2_OUT_PIN);
  escMotor3.attach(MOTOR_3_OUT_PIN);
  escMotor4.attach(MOTOR_4_OUT_PIN);

  PCintPort::attachInterrupt(MOTOR_1_IN_PIN, calcMotor1,CHANGE); 
  PCintPort::attachInterrupt(MOTOR_2_IN_PIN, calcMotor2,CHANGE); 
  PCintPort::attachInterrupt(MOTOR_3_IN_PIN, calcMotor3,CHANGE); 
  PCintPort::attachInterrupt(MOTOR_4_IN_PIN, calcMotor4,CHANGE); 
}

void loop()
{
  static int16_t unMotor1In;
  static int16_t unMotor2In;
  static int16_t unMotor3In;
  static int16_t unMotor4In;
  static int8_t bUpdateFlags;

  // check shared update flags to see if any channels have a new signal
  if(bUpdateFlagsShared)
  {
    noInterrupts(); // turn interrupts off while making local copies

    // copy of updated channels
    bUpdateFlags = bUpdateFlagsShared;
    
    if(bUpdateFlags & MOTOR_1_FLAG)
    {
      unMotor1In = unMotor1InShared;
    }
    
    if(bUpdateFlags & MOTOR_2_FLAG)
    {
      unMotor2In = unMotor2InShared;
    }
    
    if(bUpdateFlags & MOTOR_3_FLAG)
    {
      unMotor3In = unMotor3InShared;
    }
     
    if(bUpdateFlags & MOTOR_4_FLAG)
    {
      unMotor4In = unMotor4InShared;
    }

    // clear shared copy of updated flags
    bUpdateFlagsShared = 0;
    
    interrupts();
  }
  
  if(bUpdateFlags & MOTOR_1_FLAG)
  {
    if(escMotor1.readMicroseconds() != unMotor1In)
    {
      escMotor1.writeMicroseconds(unMotor1In);
    }
  }
  
  if(bUpdateFlags & MOTOR_2_FLAG)
  {
    if(escMotor2.readMicroseconds() != unMotor2In)
    {
      escMotor2.writeMicroseconds(unMotor2In);
    }
  }
  
  if(bUpdateFlags & MOTOR_3_FLAG)
  {
    if(escMotor3.readMicroseconds() != unMotor3In)
    {
      escMotor3.writeMicroseconds(unMotor3In);
    }
  }
  
  if(bUpdateFlags & MOTOR_4_FLAG)
  {
    if(escMotor4.readMicroseconds() != unMotor4In)
    {
      escMotor4.writeMicroseconds(unMotor4In);
    }
  }
  
  bUpdateFlags = 0;
}
