# INSTALLATION INSTRUCTIONS

1) Download the source code. 

2) In order to play Hangman, you will need the Graphics library. To install, 
simply nagivate to the terminal and enter
```
opam install graphics
```
If any issues arise, it may be because you are missing another package. 
Please utilize the internet to install any packages you may be missing.

3) This final step is specific to your computer's operating system.

For Windows users: Install XMing X Server at 
https://sourceforge.net/projects/xming/. Allow for XMing to be running in the 
background before beginning game.

Navigate to the directory where the file is stored and run the following code in 
the terminal to play the game:

```
source .bashrc
~make build
~make play
```

For Mac users: Install XQuartz at https://www.xquartz.org/. Allow for XQuartz to 
be running in the background before beginning game. 

Navigate to the directory where the file is stored and run the following code in 
the terminal to play the game:

```
~make build
~make play
```

If you see an error about the display being unable to open, run: 

```
export DISPLAY=:0
```

and retry the above instructions.

4) If any issues or complications arise, please contact the game creators at
bme38@cornell.edu, mms453@cornell.edu, or ew432@cornell.edu.