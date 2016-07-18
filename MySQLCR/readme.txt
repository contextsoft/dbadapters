==================================================
Context Database Extensions - MySQL Adapter
Copyright (c) 2006-2014, Context Software LLC.
All Rights Reserved
--------------------------------------------------
Version: 3.40


  This package contains freeware add-ons to Context
  Database Extensions. All software in this package comes
  with complete source code and may be freely
  redistributed and used in development of commercial
  software.


INSTALLATION INSTRUCTIONS
=========================

Before installing this package, please uninstall any previously installed 
versions from Delphi IDE. Close Delphi IDE and start it over to ensure, 
that none of the TMySQLxxxExt components are installed.

Please also delete MySQLExtPkgDX.* files from default Delphi\BDS 
BPL directory (e.g. $(DELPHI)\Projects\Bpl).

In order to install MySQL extensions components into Delphi IDE
you should:

- Make sure, that you have standard MySQL components installed in 
Delphi IDE and their path is added to the Delphi library path.

- Use "File\Open..." menu item of Delphi IDE to open design-time package 
MySQLExtPkgDx.dpk. (where 'x' is your version of Delphi). Packages 
are located in 'vcl\packages' folder of installation.

- In the "Package..." window click "Compile" button to compile the
package and then "Install" button to register the package in the 
component palette. When package is installed the components will be 
added to the 'Database Extensions' page. Delphi will automatically 
add all required, database specific, packages.

- Add path to 'vcl\source' folder to Delphi library paths.

Installing Help File
~~~~~~~~~~~~~~~~~~~~
To add component help file into Delphi/BCB IDE help system environment
and make it available in design time you need to make several steps:

1. Run Borland Delphi/CB, select item Help | Customize... from the
main menu. 

2. Choose Index tab to include index information from ctxdbext.hlp file
into Delphi common help system. Pick Edit | Add Files from menu
and then select *.hlp file located in 'vcl\help' folder in the 
File Open dialog and press OK.

3. Choose Link tab and do the same to include help topics into the Delphi
help system as well.

4. Go to Contents tab and add *.cnt file.

5. Select File | Save Project from the main menu to save changes you made
and close the OpenHelp utility.

The included help file will be available immediately within Delphi/BCB IDE.

-------------------------------------------------------------------------

If you have any questions, comments or suggestions, please
contact us at sales@contextsoft.com.

