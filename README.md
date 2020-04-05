AltTabPro
---------

AltTabPro is a replica of Windows 10 Alt-Tab task switcher with extra features like:

- Multimonitor support
- Programmer mode
- Blacklist
- Restart/Relaunch/Kill
- Carrousel mode

### Multimonitor suppor:

- Detects current application's monitor and Alt-Tab will show in that monitor
- You can stick to specific monitor if desired

### Programmer mode

I called this `programmer mode` since it means you might like command line, and thus this mode
changes the appearance and you can switch tasks by writting its names.

- Fuzzy filtered search, i.e. write some letter and any results can be recursively searched
- Command switchs to search groups, etc.

### Blacklist

You can filter out some applications from the task lis, it comes handy when you just want to focus on some
opened tasks, or hide them from visualizing like when you screencast/record/stream video 

### Restart/Relaunch/Kill

You can restart picked application using the same command line and params that was used to launch it, 
or you can start another instance if that application support multi-instance, and also you can kill its process.

Limitations: Requires elevations privileges if you want to manipulate elevated running processes.

### Carrousel mode

You can list tasks using a carrousel like animation
