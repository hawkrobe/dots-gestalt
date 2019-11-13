import random as nd

with open("orientation_pilot.txt", "w") as f :
    # Header
    f.write('<azk><cr><fd 9><t 2000><vm 1024,768,768,32,75><dbc 255255255><dwc 000000000><id "PIO12"><umb><mip 777><id keyboard><mip -Bit2><mip -Bit3><mpr -Bit2><mnr -Bit3><nfb><s 200>\n\n')

    # Instructions
    f.write('$\n0<ln -8>"In this task you will be presented with",<ln -7> "two dot displays separated by a mask.", <ln -4> "Press the BOTTOM-LEFT button if the dots in the second display", <ln -3> "are in a DIFFERENT LOCATION from the first.", <ln -2> "Press the BOTTOM-RIGHT button if they are in the SAME LOCATION.", <ln 0>  "There will be 4 blocks of trials - feel free to take a break between blocks.", <ln 1>  "BOTH SPEED AND ACCURACY ARE IMPORTANT.", <ln 3>  "You will start with a set of practice trials.", <ln 4>  "For the practice trials only, you will hear a high beep after an incorrect response", <ln 5>  "and a low beep if you took too long to respond.", <ln 6>  "You will not receive feedback for correct responses.", <ln 8>  "Press SPACEBAR to begin.";\n$\n')
    f.write('\n')
    f.write('$\n')
    f.write('0<ln -2>"We will begin with some practice trials.",\n')
    f.write('<ln +2>"Press SPACEBAR when you are ready to continue.";\n')
    f.write('$\n')
    
    # Hard coded practice trials 
    f.write('+9000<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask2" <% 18>/* <bmp>"L000000Op60P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('+9001<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask4" <% 18>/* <bmp>"L000000On30P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('-9002<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask4" <% 18>/* <bmp>"L000000Op00P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('+9003<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask1" <% 18>/* <bmp>"L000000Op15P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('-9004<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask1" <% 18>/* <bmp>"L000000Op00P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('+9005<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask5" <% 18>/* <bmp>"L000000On45P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('-9006<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask3" <% 18>/* <bmp>"L000000Op00P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('+9007<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask3" <% 18>/* <bmp>"L000000On15P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('+9008<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask3" <% 18>/* <bmp>"L000000On60P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('-9009<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask3" <% 18>/* <bmp>"L000000Op00P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('-9010<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask3" <% 18>/* <bmp>"L000000Op00P100"/<% 141>/<mwb binr,9992 bic,0 biw,9991>/;\n')
    f.write('9991 <wav 2>"wrongbeep" <bu 0>;\n')
    f.write('9992 <wav 2>"timeoutbeep" <bu 0>;\n')
    f.write('~0;\n')
    f.write('\n')
    f.write('<ln -2>"We are now ready to begin the experiment.",<ln +2>"Press SPACEBAR when you are ready to continue.";\n')
    f.write('$\n')

    # Trials
    for block_num in range(1,6) :
        start_index = (200 * (block_num - 1)) + 1
        # Print out "no change"
        for i in range(start_index,start_index+100) :
            f.write('-{0}<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask{1}" <% 18>/* <bmp>"L000000Op00P100"/<% 141>/;\n'.format(i,nd.randrange(1,5)))

        # "Change pos"
        for j in range(start_index+100,start_index+150) :
            f.write('+{0}<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask{1}" <% 18>/* <bmp>"L000000Op{2}P100"/<% 141>/;\n'.format(j,nd.randrange(1,5), (12 * block_num)))

        # "Change neg"
        for k in range(start_index+150,start_index+200) :
            f.write('+{0}<bmp>"fix"<% 15>/<% 2>/<bmp>"base"/<bmp>"mask{1}" <% 18>/* <bmp>"L000000On{2}P100"/<% 141>/;\n'.format(k,nd.randrange(1,5), (12 * block_num)))

        f.write('$\n')
        f.write('0<ln -2>"You have completed block {0} of 5.",<ln +2>"Press SPACEBAR when you are ready to continue.";\n'.format(block_num))
        f.write('$\n')

    f.write('$\n')
    f.write('0<ln -2>"You have completed the session.",<ln +2>"Thank you for your participation.";\n')
    f.write('$\n')
    
    
