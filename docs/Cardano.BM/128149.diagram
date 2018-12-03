format 221

classinstance 128021 class_ref 128405 // Controller
 name ""  xyz 353 264 2000
classinstance 128149 class_ref 128021 // Trace
 name ""  xyz 290 364 2000
classinstancecanvas 128277 classinstance_ref 128021 // User
  xyz 105 364 2000
end
classinstance 128661 class_ref 129941 // TraceContext
 name ""  xyz 514 308 2000
note 129813 "entering a log item : LogItem, the  Controller will check the minimumSeverity in the TraceContext, and the named severity filter, if available."
  xyzwh 80 113 2000 325 89
classinstance 129941 class_ref 136981 // Configuration
 name ""  xyz 510 210 2000
linkcanvas 128405
  from ref 128277 z 2001 to ref 128149
dirscanvas 129045 z 1000 linkcanvas_ref 128405
  
  forward_label "1 logInfo()" xyz 203 345 3000
linkcanvas 128533
  from ref 128149 z 2001 to ref 128021
dirscanvas 128917 z 1000 linkcanvas_ref 128533
  
  forward_label "2 checkSeverity()" xyz 263 296 3000
linkcanvas 130709
  from ref 128021 z 2001 to ref 129941
dirscanvas 130837 z 1000 linkcanvas_ref 130709
  
  forward_label "2.2 getSeverityFilter()" xyz 357 216 3000
linkcanvas 131221
  from ref 128021 z 2001 to ref 128661
dirscanvas 131349 z 1000 linkcanvas_ref 131221
  
  forward_label "2.1 minimumSeverity()" xyz 480 274 3000
msgs
  msg operation_ref 129301 // "logInfo()"
    forward ranks 1 "1" dirscanvas_ref 129045
    no_msg
  msg operation_ref 129429 // "checkSeverity()"
    forward ranks 2 "2" dirscanvas_ref 128917
    msgs
      msg operation_ref 129557 // "minimumSeverity()"
	forward ranks 3 "2.1" dirscanvas_ref 131349
	no_msg
      msg operation_ref 151317 // "getSeverityFilter()"
	forward ranks 4 "2.2" dirscanvas_ref 130837
	no_msg
    msgsend
msgsend
end
