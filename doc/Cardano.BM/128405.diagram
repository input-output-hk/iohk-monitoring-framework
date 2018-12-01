format 221

classinstance 128021 class_ref 136981 // Configuration
 name ""  xyz 472 226 2000
classinstance 128149 class_ref 129045 // ConfigurationView
 name ""  xyz 290 364 2000
classinstancecanvas 128277 classinstance_ref 128021 // User
  xyz 104 364 2000
end
note 129813 "The user can add/remove a severity filter on a name:LoggerName"
  xyzwh 80 113 2000 231 83
classinstance 129941 class_ref 130197 // LoggerName
 name ""  xyz 432 434 2000
linkcanvas 128405
  from ref 128277 z 2001 to ref 128149
dirscanvas 129045 z 1000 linkcanvas_ref 128405
  
  forward_label "1 addSeverityFilter()
3 removeSeverityFilter()" xyz 160 328 3000
linkcanvas 128533
  from ref 128149 z 2001 to ref 128021
dirscanvas 128917 z 1000 linkcanvas_ref 128533
  
  forward_label "2 setSeverityFilter()
4 setSeverityFilter()" xyz 348 263 3000
linkcanvas 130069
  from ref 128149 z 2001 to ref 129941
msgs
  msg operation_ref 128277 // "addSeverityFilter()"
    forward ranks 1 "1" dirscanvas_ref 129045
    msgs
      msg operation_ref 151189 // "setSeverityFilter()"
	forward ranks 2 "1.1" dirscanvas_ref 128917
	no_msg
    msgsend
  msg operation_ref 128533 // "removeSeverityFilter()"
    forward ranks 3 "2" dirscanvas_ref 129045
    msgs
      msg operation_ref 151189 // "setSeverityFilter()"
	forward ranks 4 "2.1" dirscanvas_ref 128917
	no_msg
    msgsend
msgsend
line 130197 -_-_
  from ref 128149 z 2001 to ref 129941
end
