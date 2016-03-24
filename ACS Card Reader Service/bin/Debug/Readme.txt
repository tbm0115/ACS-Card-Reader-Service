//////////////////////////////////////////////////
//
// ACS Card Reader Service
// version 1.0.1
//
//////////////////////////////////////////////////

ACS Card Reader Service is a Windows Service running on 
.NET v4.0+. Please ensure you have the latest version of the
.NET framework installed on your machine.

The ACS Card Reader Service targets the first smart card reader available to WinsCard and regularly (every 250ms) queries the card reader for a card. If a card/device is contacted, all verified blocks of data will be read and sent in a pipe-delimited string along the named pipe of 'ACS' where any application can read the stream.

== To install ==
 - Run the 'Install.bat' as Administrator from somewhere on your machine (move the file anywhere on C:\)
	(Right-Click -> 'Run as Administrator')

== To verify installation ==
 - Open 'Component Services' in Windows
 - Expand/Click on 'Services (Local)'
 - Search for the name 'ACS Card Reader'

== Troubleshooting the service ==
 - Open 'Component Services' in Windows
 - Expand/Click on 'Event Viewer (Local)'
 - Expand/Click on 'Applications and Services Logs'
 - Click on 'CardWatcherLog'
 - Cycle through the messages to troubleshoot

== To uninstall ==
 - Run the Uninstall.bat' as Administrator from somewhere on your machine (move the file anywhere on C:\)

== Updates/Notes ==
= v1.0.1 = 
 - Added custom command functionality (Sends return through 'ACS' pipe)
  - 128 = ReadAll data. Returns pipe-delimited string.
  - 129 = Check if card is fully connected. Returns True/False.
  - 130 = Gets the card/device's UID. Returns UID hex string.
= v1.0.0 =
 - Build released
 - Known issue with only two verified block (&H04,&H05)
 - Known issue with persistant smart card state