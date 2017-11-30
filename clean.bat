del /q /s bin\Demo1
del /q /s bin\Demo2
del /q /s bin\Demo3

rd /s /q Common\__history
rd /s /q Common\__recovery

del /q Demo1\CopyDBTables.groupproj.local
rd /s /q Demo1\Mobile\__history
rd /s /q Demo1\Mobile\__recovery
rd /s /q Demo1\Mobile\Win32
del /q Demo1\Mobile\FMXClient.dproj.local
del /q Demo1\Mobile\FMXClient.identcache
del /q Demo1\Mobile\FMXClient.stat
del /q Demo1\Mobile\FMXClientBeaconCast.identcache
del /q Demo1\Mobile\FMXClientBeaconCast.stat
rd /s /q Demo1\Server\__history
rd /s /q Demo1\Server\__recovery
rd /s /q Demo1\Server\Win32
del /q Demo1\Server\VCLServerDesktop.dproj.local
del /q Demo1\Server\VCLServerDesktop.identcache
del /q Demo1\Server\VCLServerDesktop.stat

del /q Demo2\CopyDBTables.groupproj.local
rd /s /q Demo2\Mobile\__history
rd /s /q Demo2\Mobile\__recovery
rd /s /q Demo2\Mobile\Win32
del /q Demo2\Mobile\FMXClient.dproj.local
del /q Demo2\Mobile\FMXClient.identcache
del /q Demo2\Mobile\FMXClient.stat
del /q Demo2\Mobile\FMXClientBeaconCast.identcache
del /q Demo2\Mobile\FMXClientBeaconCast.stat
rd /s /q Demo2\Server\__history
rd /s /q Demo2\Server\__recovery
rd /s /q Demo2\Server\Win32
del /q Demo2\Server\VCLServerDesktop.dproj.local
del /q Demo2\Server\VCLServerDesktop.identcache
del /q Demo2\Server\VCLServerDesktop.stat

del /q Demo3\CopyDBTables.groupproj.local
rd /s /q Demo3\Mobile\__history
rd /s /q Demo3\Mobile\__recovery
rd /s /q Demo3\Mobile\Win32
del /q Demo3\Mobile\FMXClient.dproj.local
del /q Demo3\Mobile\FMXClient.identcache
del /q Demo3\Mobile\FMXClient.stat
del /q Demo3\Mobile\FMXClientBeaconCast.identcache
del /q Demo3\Mobile\FMXClientBeaconCast.stat
rd /s /q Demo3\Server\__history
rd /s /q Demo3\Server\__recovery
rd /s /q Demo3\Server\Win32
del /q Demo3\Server\VCLServerDesktop.dproj.local
del /q Demo3\Server\VCLServerDesktop.identcache
del /q Demo3\Server\VCLServerDesktop.stat

Pause