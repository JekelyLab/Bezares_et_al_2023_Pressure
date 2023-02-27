

//Pin variables
const int camera  =  9; //use digital I/O pin 8. The 'const' adjetive is used to avoid unintendely changing the pin number.
const int ValveIN = 7;
const int ValveOUT = 8;
const int sensorPin = A0;  
const int UVlightPin=12; 
//Timer variables
unsigned long BEGINNING_LOG=1000;
unsigned long BEGINNING_PROTOCOL=5000;//This flag sets the beginning of the protocol and of the recording at 5s.
unsigned long INCREASE_PRESURE=35000; // 1 min after start pressure should start increasing
unsigned long DECREASE_PRESURE=65000; 
unsigned long UV_ON=15000; 
unsigned long UV_OFF=135000;


//Parameters
float TestVoltage= 3.9; // The target voltage at which the solenoid valve has to swtich off.
float basalVoltage= 0.45;  // The voltage limit before stimulus is applied. 0.47 for 50,100,250mb T, 0 for 1000mb
unsigned long Totaldelay=0;
int TransdSensit=100; //indica te max value of pressure transducer. 
int Drange=400;//500 for 1000mb transducer
bool UVprotocol= false;
bool Pressure_protocol=true;


//Variables
unsigned long time_now=0; // variable to  capture the value of millis()
unsigned long frame=0; // variable to  indicate the frame number 
int PrevdigitalVal = 0; 
float AnalogVoltage = 0.00; // average voltage calculated from the average digital value
bool title= false; // Flag to indicate if the pressure stimulation protocol has started.
float Unitstolerance= 0.01*(2.5*Drange)/TransdSensit; //a tolerance of max. 5mb change is defined in terms of voltage units.
int UV_state=0;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600); ////use 115200 for  file writing. 9600 for serial monitor.
  pinMode(camera,OUTPUT);   
  pinMode(ValveIN, OUTPUT);
  pinMode(ValveOUT, OUTPUT);
  pinMode(UVlightPin, OUTPUT);
  Totaldelay=20; //50 for frame averaging, 100 when not
}

void loop() {
  time_now=millis();
  if(time_now > BEGINNING_LOG){
    if(title==false){
      Serial.println("time\tframe\tPresV\tUV");
      title=true;
    }
    if(time_now >  BEGINNING_PROTOCOL+ Totaldelay*frame){
      CameraTrigger();
    }
  //  Data getvalues=MeasureVoltage(PrevdigitalVal,sensorPin);  //Calling function to calculate average voltage.
//  PrevdigitalVal=getvalues.D;
//  AverageanalogVoltage=getvalues.V;
    AnalogVoltage=analogRead(sensorPin);
    AnalogVoltage=(AnalogVoltage * 4.50)/1024.00;
    if(Pressure_protocol==true){
      if(time_now > INCREASE_PRESURE && time_now < DECREASE_PRESURE){
        ValveTrigger(AnalogVoltage,TestVoltage);
      }
      else{
        ValveTrigger(AnalogVoltage,basalVoltage);
      }
    }
    Serial.print(time_now);
    Serial.print("\t");
    Serial.print(frame);
    Serial.print("\t");
    Serial.print(AnalogVoltage);
    Serial.print("\t");
    Serial.println(UV_state);
    if(UVprotocol==true){
      if(time_now > UV_ON && time_now < UV_OFF){
        digitalWrite(UVlightPin, HIGH);
        UV_state=1;
      }else{
        digitalWrite(UVlightPin, LOW);
        UV_state=0;
      }
    }
  }   
}

void CameraTrigger(){
    digitalWrite(camera,HIGH);
    delayMicroseconds(3); //The 5V pulse has to be > 2.5us for ImagingSource camera.
    digitalWrite(camera,LOW);
    frame++;
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
