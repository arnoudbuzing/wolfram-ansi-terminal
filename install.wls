Print[];
paclets = PacletFind @ "AnsiTerminal";
Print["Found the following old paclets (uninstalling): ", Map[#["Location"]&, paclets]];
Map[PacletUninstall,paclets];

directory = DirectoryName @ $InputFileName;
Print["Using directory: ",directory];
files = FileNames["*.paclet", directory];
Print["Paclet file(s) found: ", files];
paclet = Last @ Sort @ files;
Print["Selecting paclet: ", paclet];
PacletInstall[paclet,IgnoreVersion->True];
RebuildPacletData[];
