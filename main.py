
import time
import matplotlib.pyplot as plt
import serial

THRESHOLD = 40

ser = serial.Serial(
    port='COM6',
    baudrate=19200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_ONE,
    bytesize=serial.EIGHTBITS,
    timeout=0)

print("connected to: " + ser.portstr)
x=[]
x2=[]
x3=[]
x4=[]
count=0
for i in range(20):
    x.append(i)
while count<60:
    for line in ser.read():
        if count % 3 == 1:
            count += 1
            x2 .append( max(int(line)-THRESHOLD,0))
        elif count % 3 == 2:
            count += 1
            x3 .append( max(int(line)-THRESHOLD,0))
        else:
            count += 1
            x4.append(max(int(line)-THRESHOLD,0))
        time.sleep(0.1)

print("S-a terminat citirea datelor")
plt.plot(x,x2, label ="x")
plt.plot(x,x3, label ="y")
plt.plot(x,x4, label="z")
plt.title('Grafic Aceleratie')
plt.xlabel('timp(s)')
plt.ylabel('acceleratie')
print(x2)
print(x3)
print(x4)
plt.legend()
plt.show()
ser.close()
