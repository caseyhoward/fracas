module Maps.MobileFriendlier exposing (map)


map : String
map =
    """
;Fracas 2.0 by Jason Merlo
;http://www.smozzie.com
;jmerlo@austin.rr.com
Created=9/18/2019 11:39:33 PM

;Terraform
CountrySize=30
CountryProportions=10
CountryShapes=68
MinLakeSize=16
LandPct=80
Islands=2

;Options
InitTroopPl=5
InitTroopCt=1
BonusTroops=2
Ships=3
Ports=3
Conquer=1
Events=1
1stTurn=2
HQSelect=1

;Preferences
Borders=5
Sound=2
AISpeed=1
AnimSpeed=1
Explosions=1
Waves=1
UnoccupiedColor=6
Flashing=1
Resolution=1
Prompt=1

;Players
Player1=Bowie,10,1,1
Player2=Pinky,8,2,6
Player3=Mac Dandy,11,2,6
Player4=Ozzie,2,2,6
Player5=Lady,4,2,6
Player6=Chula,3,2,6

{Menu and Map Data}
C:\\Program Files\\Fracas\\Maps\\FracasMobileFriendlier.map
 84 
 1011 
{Map}
1001.1001.1001.1001.1001.83.83.83.83.83.83.54.54.54.41.41.41.41.41.41.41.41.41.41.41.41.44.44.44.44.44.44.44.44.65.65.65.65.71.71.80.80.80.80.80.80.80.80.80.84.84.84.84.75.75.75.1008.1008.1008.1008.1008.1008.1008.1008.1008.1008.
1001.1001.1001.1001.1001.1001.1001.83.83.83.83.54.54.54.54.41.41.1003.41.41.41.41.41.41.41.41.44.44.44.44.44.44.44.44.65.65.65.71.71.71.71.71.80.80.80.80.84.84.84.84.84.84.84.75.75.75.75.75.1008.75.75.1008.1008.1008.1008.1008.
1001.1001.1001.1001.79.1001.1001.1001.83.83.83.54.54.54.54.54.54.1003.41.41.41.41.29.41.41.29.44.44.44.44.44.44.44.65.65.65.65.65.65.71.71.71.80.80.80.80.80.80.80.84.84.84.84.75.75.75.75.75.75.75.75.75.75.1008.1008.1008.
1001.79.79.79.79.1001.1001.83.1001.83.83.83.54.54.54.54.1003.41.41.41.41.29.29.29.41.29.29.29.44.44.44.44.44.65.65.65.65.65.65.71.71.71.80.80.80.80.80.84.84.84.84.84.84.75.75.68.75.75.75.75.75.1008.1008.1008.1008.59.
1001.79.79.79.79.79.79.83.83.83.83.83.54.54.54.1003.1003.55.55.29.29.29.29.29.29.29.29.29.29.44.44.65.65.65.65.65.65.65.65.71.71.71.80.80.80.80.84.84.84.1007.1007.1007.75.75.75.68.68.68.68.68.1008.1008.59.1008.59.59.
1001.1001.79.79.79.79.62.83.83.83.83.83.54.54.54.1003.1003.55.55.55.55.29.29.29.29.29.29.29.29.29.29.65.65.65.65.65.72.72.71.71.71.71.71.80.80.80.84.84.84.84.1007.1007.75.68.68.68.68.68.68.68.68.1008.59.1008.59.59.
79.79.79.79.79.79.62.62.62.62.83.83.54.54.54.1003.1003.55.55.55.55.29.29.29.12.12.12.12.70.29.70.65.65.65.65.65.72.72.71.71.71.71.71.80.82.82.82.82.1007.1007.1007.1007.1007.68.68.68.68.68.68.68.68.59.59.59.59.59.
79.79.79.79.79.62.62.62.62.62.62.54.54.54.54.1003.55.55.55.55.55.55.29.29.12.12.12.70.70.70.70.70.70.65.65.72.72.72.72.71.71.71.71.82.82.82.82.1007.82.1007.1007.1007.1007.68.68.68.68.68.68.59.59.59.59.59.59.59.
79.1002.79.62.62.62.62.62.62.62.62.62.54.54.1003.1003.55.55.55.55.55.12.12.12.12.12.12.70.70.70.70.70.70.70.70.70.72.72.72.72.72.82.82.82.82.82.82.82.82.82.1007.1007.1007.1007.68.58.58.58.58.58.59.59.59.59.59.59.
1002.1002.79.1002.62.62.62.62.62.43.62.54.54.1003.1003.1003.55.55.55.55.55.12.12.12.12.12.12.70.70.70.70.70.70.45.72.72.72.72.72.72.72.82.82.82.82.82.82.82.82.82.1007.1007.1007.1007.68.58.58.58.58.58.58.58.58.59.59.59.
1002.1002.1002.1002.62.62.43.43.43.43.43.43.38.1003.1003.1003.55.55.55.55.55.12.12.12.12.12.12.70.70.70.70.70.70.45.45.45.72.72.72.72.81.81.81.81.82.82.81.1007.1007.1007.1007.56.1007.1007.1007.56.58.58.58.58.58.58.58.58.58.58.
1002.1002.1002.1002.62.62.43.43.43.43.43.38.38.1003.1003.35.1003.55.35.35.35.12.12.12.12.12.16.16.45.45.70.45.45.45.45.45.72.72.72.72.81.81.81.81.82.81.81.81.1007.1007.1007.56.56.56.56.56.56.56.58.58.58.67.67.67.67.67.
1002.1002.1002.1002.1002.43.43.43.43.43.43.38.38.38.1003.35.35.1003.35.35.35.35.35.12.16.16.16.16.16.45.45.45.45.45.45.45.61.72.72.72.72.81.81.81.81.81.81.81.81.1007.1007.56.56.56.56.56.56.56.58.58.58.67.67.67.67.67.
1002.1002.1002.1002.43.43.43.43.43.43.38.38.38.38.38.38.35.35.35.35.35.35.35.35.16.16.16.16.16.45.45.45.45.45.45.45.61.61.61.57.81.81.81.81.81.81.81.1007.81.1007.1007.56.56.56.56.56.56.56.73.58.73.67.67.67.67.67.
1002.1002.1002.1002.1002.43.43.43.43.38.38.38.38.38.38.35.35.22.22.22.35.35.35.35.16.16.16.16.16.16.16.45.45.45.45.45.61.61.57.57.57.57.57.81.81.57.1007.1007.1007.1007.1007.56.56.56.56.56.56.73.73.73.73.67.67.1009.67.67.
1002.1002.1002.1002.1002.1002.43.43.42.42.38.38.38.38.38.35.35.22.22.22.22.35.35.35.16.16.16.16.24.16.16.45.45.45.24.24.61.61.57.57.57.57.57.81.81.57.57.57.1007.1007.1007.20.20.20.20.20.73.73.73.73.73.67.67.67.1009.67.
1002.1002.1002.1002.1002.1002.1002.42.42.42.42.38.38.38.38.22.22.22.22.22.22.22.22.35.16.16.16.16.24.24.24.24.24.24.24.24.24.61.61.61.61.57.57.57.57.57.57.57.26.26.26.20.20.20.20.73.73.73.73.73.73.73.67.1009.1009.67.
1002.1002.1002.1002.1002.42.42.42.42.42.42.38.38.38.22.22.22.22.22.17.22.22.22.17.17.16.16.24.24.24.24.24.24.24.24.24.24.61.61.61.61.57.57.57.57.57.57.26.26.26.26.20.20.20.20.20.73.73.39.73.73.73.67.1009.1009.67.
1002.1002.1002.1002.42.42.42.42.42.42.30.30.38.38.30.22.22.22.22.17.17.22.17.17.17.16.16.24.24.24.24.24.34.24.24.24.24.61.61.61.61.57.57.57.57.57.57.26.26.26.26.20.20.20.20.20.20.20.39.39.39.73.67.1009.1009.67.
1002.1002.1002.42.42.42.42.42.42.42.30.30.30.30.30.22.22.22.22.17.17.17.17.17.17.16.16.24.24.24.24.24.34.34.34.34.24.61.61.61.61.57.57.32.57.32.26.26.26.26.26.20.20.20.20.39.39.39.39.39.73.73.73.1009.1009.67.
1002.1002.1002.42.42.42.42.42.42.30.30.30.30.30.22.22.25.25.22.25.17.17.17.17.17.17.17.17.17.24.24.24.34.34.34.34.61.61.61.61.61.32.32.32.32.32.32.26.26.26.26.26.20.20.20.39.39.39.39.39.39.73.73.1009.1009.1009.
64.64.36.36.36.42.36.36.36.36.30.30.30.30.30.30.25.25.25.25.17.17.17.8.8.17.17.17.52.52.52.34.34.34.34.34.34.61.61.61.32.32.32.32.27.32.26.26.26.26.15.26.26.20.20.39.39.39.39.39.39.73.1009.1009.1009.1009.
64.64.64.36.36.36.36.36.36.30.30.30.30.30.30.25.25.25.25.25.17.17.25.8.8.8.8.8.8.52.34.34.34.34.34.69.69.61.61.61.32.32.32.32.27.32.32.15.26.15.15.15.26.26.20.39.39.39.39.39.19.1009.1009.1009.1009.1009.
64.64.64.64.64.36.36.36.36.36.30.30.18.30.25.25.25.25.25.25.25.25.25.25.8.8.8.8.52.52.52.52.34.34.34.69.69.69.61.32.32.32.32.27.27.27.15.15.15.15.15.15.26.26.20.39.39.39.39.19.19.1009.1009.1009.1009.78.
64.64.64.64.36.36.36.36.36.36.36.36.18.18.18.18.25.25.25.25.4.25.25.25.25.8.8.8.8.52.52.52.34.34.34.34.69.69.61.61.32.32.27.27.27.27.15.15.15.15.15.15.26.26.10.10.10.10.10.19.19.19.1009.1009.1009.78.
64.64.21.21.21.21.21.36.36.36.36.18.18.18.18.18.3.25.25.25.4.4.4.4.8.8.8.8.8.52.52.52.52.34.34.34.69.69.69.61.32.32.32.27.27.27.27.15.15.15.15.15.10.10.10.10.10.10.10.19.19.19.78.78.78.78.
64.64.64.21.21.21.21.21.18.36.18.18.18.18.18.18.3.3.3.25.4.4.4.4.4.8.8.8.52.52.52.52.52.52.34.69.69.69.69.61.32.32.32.27.27.27.27.27.15.15.15.10.10.10.10.10.10.10.19.19.19.19.19.78.78.78.
64.64.21.21.21.21.21.21.18.18.18.18.18.18.18.18.3.3.3.3.3.4.4.4.4.8.8.8.8.52.52.52.52.52.52.69.69.69.69.69.77.77.77.77.27.27.27.15.15.15.15.10.10.10.10.10.10.19.19.19.19.19.19.78.78.78.
64.64.21.21.21.13.21.21.18.13.18.18.18.18.18.1.1.3.3.3.3.3.4.4.4.4.8.1005.1005.52.52.52.52.1005.69.69.69.69.69.69.69.77.77.77.27.27.27.15.15.9.9.9.9.9.10.10.10.19.19.19.19.19.78.78.78.78.
64.64.21.21.21.13.21.13.18.13.13.18.7.18.18.1.1.1.3.3.3.3.4.4.4.4.4.1005.1005.1005.1005.52.1005.1005.1005.69.69.69.77.77.77.77.77.77.27.27.27.15.9.9.9.9.9.9.9.9.10.11.11.11.19.19.19.78.78.78.
64.21.21.21.13.13.13.13.13.13.13.7.7.1.1.1.1.1.3.3.3.3.3.4.4.4.4.4.1005.28.1005.1005.1005.74.1005.69.77.77.77.77.77.77.77.27.27.27.27.15.15.9.9.9.9.9.9.11.11.11.11.11.11.19.78.78.78.78.
64.64.21.21.13.13.13.13.13.13.7.7.7.1.1.1.1.1.3.3.3.3.3.60.60.4.28.28.28.28.28.1005.1005.74.74.74.74.74.74.77.77.77.77.27.2.2.2.2.15.15.9.9.9.9.9.9.11.11.11.11.11.78.78.78.78.78.
64.47.21.13.13.13.13.13.13.7.7.7.7.7.1.1.1.1.3.3.3.3.3.60.60.28.28.28.28.28.28.28.74.74.74.74.74.74.74.74.77.77.77.77.77.2.2.2.2.9.9.6.9.9.11.11.11.11.11.11.11.11.48.48.78.78.
64.47.47.13.13.13.13.13.7.7.7.7.5.1.1.1.1.1.1004.1004.1004.3.60.60.60.28.28.28.28.28.28.49.49.49.74.74.74.74.74.74.66.66.66.77.77.2.2.2.2.2.2.6.6.9.9.11.11.11.11.48.48.48.48.48.48.78.
64.47.47.47.13.13.23.7.7.7.7.5.5.1.1.1.1004.1004.1004.1004.1004.3.60.60.60.28.28.28.28.28.28.28.49.49.49.49.74.74.74.74.66.66.66.77.77.2.2.2.2.6.2.6.6.6.6.11.11.11.48.48.48.48.48.48.48.48.
47.47.47.23.23.23.23.23.7.7.7.7.5.1.1.1.1004.1004.1004.1004.1004.1004.60.60.60.28.28.28.28.28.49.49.49.49.49.49.74.74.74.74.66.66.31.31.31.2.2.2.2.6.6.6.6.6.6.6.11.11.11.11.11.14.48.48.48.48.
47.47.47.47.23.23.23.23.7.7.7.5.5.5.5.5.5.1004.1004.1004.1004.60.60.60.60.28.28.28.28.49.49.49.49.49.49.63.63.63.74.74.66.66.66.31.31.31.2.2.2.6.6.6.6.6.6.6.14.14.14.14.14.14.48.48.48.48.
47.47.47.47.47.23.23.23.23.7.7.5.5.5.5.5.1004.1004.1004.1004.1004.1004.60.60.60.60.60.60.28.28.49.49.49.49.49.63.63.63.63.66.66.66.66.66.31.31.31.2.2.6.6.6.6.6.6.14.14.14.14.14.14.14.14.48.48.48.
47.47.47.47.47.23.23.23.23.7.5.5.5.5.5.5.5.51.1004.51.51.1004.1004.60.60.60.60.60.28.28.49.49.49.49.49.63.63.63.63.63.66.66.66.66.31.31.31.2.2.31.31.37.6.6.6.6.14.14.14.14.14.14.14.14.48.48.
47.47.47.47.47.47.23.23.23.7.50.5.5.5.5.5.5.51.51.51.51.53.1004.53.53.60.60.28.28.28.49.49.1006.1006.1006.63.63.63.63.66.66.66.66.66.31.31.31.31.31.31.31.37.6.6.6.6.14.14.14.14.14.14.1010.1010.48.48.
47.40.40.40.23.23.23.23.23.50.50.50.50.5.5.5.51.51.51.51.53.53.53.53.53.53.53.28.76.76.1006.1006.1006.1006.1006.63.63.63.63.66.66.66.66.66.31.31.31.31.31.31.37.37.6.6.37.37.14.33.14.33.33.33.1010.1010.1010.1010.
40.40.40.40.23.23.23.50.50.50.50.50.50.50.5.51.51.51.51.53.53.53.53.53.53.53.76.76.76.76.76.1006.1006.1006.63.63.63.63.1006.46.66.66.46.46.46.31.31.31.31.37.37.37.37.37.37.33.33.33.33.33.33.33.1010.1010.1010.1010.
40.40.40.40.23.23.40.40.40.50.50.50.50.50.50.51.51.51.51.53.53.53.53.53.53.76.76.76.76.76.76.1006.1006.1006.1006.63.63.1006.46.46.46.46.46.46.46.46.46.46.46.37.37.37.37.37.37.33.33.33.33.33.33.33.33.1010.1010.1010.
40.40.40.40.40.40.40.40.40.40.50.50.50.50.51.51.51.51.53.53.53.53.76.76.53.76.76.76.76.76.76.1006.1006.1006.1006.63.63.63.1006.46.46.46.46.46.46.46.46.46.46.37.37.37.37.37.37.33.33.33.33.33.33.33.1010.1010.1010.1010.
40.40.40.40.40.40.40.40.50.50.50.50.50.50.51.51.51.51.51.51.53.53.53.76.76.76.76.76.76.76.76.1006.1006.1006.1006.1006.1006.1006.1006.1006.46.46.46.46.46.46.46.46.37.37.37.37.37.37.37.37.33.33.33.33.33.33.1010.1010.1010.1010.
{Country Names}
Blania
Freburg
Hogi
Mettary
Hawutica
Quil
Zerrania
Enage
Bleton
North Otwage
Quimmi
Diggu
Delia
Deme
Grage
Fladi
Vatwi
San Oggeb
Joctary
Kreghia
South Josania
Mysiktary
Udypu
Brica
Lioway
Unephe
Tarania
Tyfo
Adwiodia
Zheja
Jepenelia
Odynica
Ifri
New Zectio
Dydah
Prug
Igittage
Queddica
Quiway
Mebany
Jedwia
New Ipia
Lower Appot
Anany
Asmilica
Grat
Olcica
Kligsia
Smet
Frei
Drifse
Ponne
Quidwu
Joponyton
Juttary
Jetia
Perne
Lower Henany
San Haidary
Zhidrary
Annelia
Quatia
Trocica
Zhesti
Zhithary
Gruland
Fedeji
Ania
Jutses
Ragsas
Holand
San Zhiway
Tyland
Wobevsary
Veddia
Stewtany
Jidre
Upia
Krorru
Plotia
Arage
Etania
Quaccary
Helia
{Water Names}
The Bay of Skir
The Bay of Igrek
Lake Bykia
Sylia Lake
The Sea of Trej
Eftafia Harbor
The Sea of Alypania
Eglatania Ocean
Vlen Ocean
The Bay of Prania
Smondeton Sea
{Hi Scores}
HI1=Penny,91,0
HI2=Queenie,83,0
HI3=Ozzie,73,0
HI4=Bowie,61,0
HI5=Smudge,51,0
HI6=Lady,46,0
HI7=BooBoo,36,0
HI8=Pinky,22,0
HI9=Mac,17,0
HI10=Zack,1,0

  """