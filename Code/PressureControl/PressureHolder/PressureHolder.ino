

//Pin variables
const int ValveIN = 7;
const int ValveOUT = 8;
const int sensorPin = A0;  

//Timer variables
unsigned long BEGINNING_LOG=1000;
unsigned long BEGINNING_PROTOCOL=60000;//This flag sets the beginning of the protocol and of the recording at 5s.
unsigned long INCREASE_PRESURE=70000; // 1 min after start pressure should start increasing
unsigned long DECREASE_PRESURE=130000; 
; 

//Parameters
float basalVoltage= 2.1;  // The voltage limit before stimulus is applied.
int TransdSensit=250; //indicate max value of pressure transducer. 

//Variables
unsigned long time_now=0; // variable to  capture the value of millis()
float AnalogVoltage = 0.00; // average voltage calculated from the average digital value
float Unitstolerance= 0.01*(2.5*400)/TransdSensit; //a tolerance of max. 5mb change is defined in terms of voltage units.

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600); ////use 115200 for  file writing. 9600 for serial monitor.
  pinMode(ValveIN, OUTPUT);
  pinMode(ValveOUT, OUTPUT);
}

void loop() {
  AnalogVoltage=analogRead(sensorPin);
  AnalogVoltage=(AnalogVoltage * 4.50)/1024.00;
  ValveTrigger(AnalogVoltage,basalVoltage); 
  Serial.print("Current_Voltage: ");
  Serial.print("\t");
  Serial.println(AnalogVoltage);  
}

void ValveTrigger(float PressVal, float TargetVoltage){
    float lowerLimitTolerance= TargetVoltage - Unitstolerance;
    float higherLimitTolerance = TargetVoltage + Unitstolerance;
    if(PressVal <= lowerLimitTolerance){
      digitalWrite(ValveIN, HIGH);
      digitalWrite(ValveOUT, LOW);
    } 
    if(PressVal >= lowerLimitTolerance && PressVal <= higherLimitTolerance){
      digitalWrite(ValveIN, LOW); 
      digitalWrite(ValveOUT, LOW);
    }
    if(PressVal >= higherLimitTolerance){
      digitalWrite(ValveIN, LOW);
      digitalWrite(ValveOUT, HIGH);
    }
}
