format 221

classinstance 128021 class_ref 130325 // Counter
 name ""  xyz 718 189 2000
classinstance 128149 class_ref 128789 // STMObserver
 name ""  xyz 451 320 2000
classinstance 128277 class_ref 129301 // Counters
 name ""  xyz 560 191 2000
classinstance 128789 class_ref 128021 // Trace
 name ""  xyz 321 202 2000
classinstancecanvas 128917 classinstance_ref 128021 // Setup
  xyz 147 316 2000
end
classinstancecanvas 129045 classinstance_ref 128149 // STM
  xyz 682 321 2000
end
note 130453 "an STM action is bracketed by reading the counters before and after its evaluation. The difference is sent to the Trace."
  xyzwh 91 55 2000 287 95
linkcanvas 128405
  from ref 128277 z 2001 to ref 128021
dirscanvas 128533 z 1000 linkcanvas_ref 128405
  
  forward_label "3 read()
6 read()" xyz 650 154 3000
linkcanvas 128661
  from ref 128149 z 2001 to ref 128277
dirscanvas 129685 z 1000 linkcanvas_ref 128661
  
  forward_label "2 readCounters()
5 readCounters()
7 calcDiff()" xyz 442 209 3000
linkcanvas 129173
  from ref 128149 z 2001 to ref 129045
dirscanvas 129813 z 1000 linkcanvas_ref 129173
  
  forward_label "4 atomically" xyz 581 301 3000
linkcanvas 129301
  from ref 128149 z 2001 to ref 128789
dirscanvas 130069 z 1000 linkcanvas_ref 129301
  
  forward_label "8 logInfo()" xyz 375 290 3000
linkcanvas 129429
  from ref 128917 z 2001 to ref 128149
dirscanvas 129557 z 1000 linkcanvas_ref 129429
  
  forward_label "1 observeIO()" xyz 301 299 3000
msgs
  msg operation_ref 128917 // "observeIO()"
    forward ranks 1 "1" dirscanvas_ref 129557
    msgs
      msg operation_ref 128789 // "readCounters() : Counter"
	forward ranks 2 "1.1" dirscanvas_ref 129685
	msgs
	  msg operation_ref 129045 // "read()"
	    forward ranks 3 "1.1.1" dirscanvas_ref 128533
	    no_msg
	msgsend
      explicitmsg "atomically"
	forward ranks 4 "1.2" dirscanvas_ref 129813
	no_msg
    msgsend
  msg operation_ref 128789 // "readCounters() : Counter"
    forward ranks 5 "2" dirscanvas_ref 129685
    msgs
      msg operation_ref 129045 // "read()"
	forward ranks 6 "2.1" dirscanvas_ref 128533
	no_msg
    msgsend
  msg operation_ref 136981 // "calcDiff(in other : Counter) : Counters"
    forward ranks 7 "3" dirscanvas_ref 129685
    no_msg
  msg operation_ref 129301 // "logInfo()"
    forward ranks 8 "4" dirscanvas_ref 130069
    no_msg
msgsend
end
