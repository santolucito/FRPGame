
(
// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary

// boot the server and start SuperDirt
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	//~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	~dirt.loadSoundFiles(thisProcess.nowExecutingPath.dirname +/+ "/samples/*");
	// s.sync; // optionally: wait for samples to be read
	~dirt.start(57120, [0, 0]);   // start listening on port 57120, create two busses each sending audio to channel 0

	// optional, needed for the sclang tests only:
	(

		~d1 = ~dirt.orbits[0]; // one orbit
		~d2 = ~dirt.orbits[1];

	);

        ~exitListener = OSCFunc( { | msg, time, addr, port |
            0.exit;
         }, '/haskellExit' , recvPort: 57122);


};

s.latency = 0.6; // increase this if you get "late" messages
);

// TODO can i use an osc listener to check for a special message (to be sent from haskell) that will s.freeAll
// then i can end the sclang process safely
