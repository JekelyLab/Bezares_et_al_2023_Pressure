
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 30 16:46:49 2019

@author: LuisBezares. Copied from https://problemsolvingwithpython.com/11-Python-and-External-Hardware/11.04-Reading-a-Sensor-with-Python/
and from https://electronics.stackexchange.com/questions/54/saving-arduino-sensor-data-to-a-text-file
"""
import serial


file= "261220_E001T_16.txt";
Full_path="/Users/LuisBezares/ownCloud/Lab/OneDrive - University of Exeter/Behavior/Pressure/Recording_logs/261220/Processed_files/Copsin1/";
serial_port = '/dev/cu.usbmodemFA131';#
baud_rate = 9600; #In arduino, Serial.begin(baud_rate)
Full_file= Full_path + file
#output_file = open(Full_file, "w+");
ser=serial.Serial(serial_port,baud_rate) #make sure the serial port in arduino is not open.
#time.sleep(2)
data =[]
with open(Full_file,encoding="utf-8", errors='ignore', mode="w") as output_file:
    for i in range(6930): #12700 for 250s or 5000f;5500 for 2000f,6930 for 2600  for long exp. 10100 for 2600frames,# X triggers per secondX Y seconds length of experiment.
        b=ser.readline()
        string_n= b.decode()
        string = string_n.rstrip()
        data.append(string)
        output_file.write(str(string)+"\n")
        #time.sleep(0.1)
ser.close()