This projects provides a library that helps to implement Monarchy-Clients in CHICKEN scheme.


== Dependencies

This project depends on the unofficial EDN implementation for CHICKEN which is not eggified but must be 
downloaded from Bitbucket instead (user DerGuteMoritz).

Apart from that it obviously needs CHICKEN scheme to be compiled. 


== Installation

Check out the Monarchy source change into the directory of the client and run: chicken-install
This will create an extension that can be used in your clients. 
See test-client.scm in the source directory to get an example of a working client.
