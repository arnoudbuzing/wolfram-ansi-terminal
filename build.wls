Print[];
directory = DirectoryName @ First @ $ScriptCommandLine;
Print["Directory: ", directory];
files = FileNames["*.paclet", directory];
Print["Found paclets (to be deleted): ", files];
DeleteFile /@ files;
Print["Building new paclet"];
paclet = PackPaclet @ FileNameJoin @ {directory, "AnsiTerminal"};
Print["New paclet: ", paclet];
