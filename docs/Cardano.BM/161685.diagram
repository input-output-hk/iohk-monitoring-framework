format 221

classinstance 128021 class_ref 136981 // Configuration
 name ""  xyz 243 108 2000
classinstancecanvas 128149 classinstance_ref 128021 // Setup
  xyz 104 195 2000
end
classinstance 128533 class_ref 144277 // Switchboard
 name ""  xyz 343 193 2000
classinstance 128917 class_ref 129173 // EKGView
 name ""  xyz 470 108 2000
classinstance 129045 class_ref 128917 // Aggregation
 name ""  xyz 501 193 2000
classinstance 129173 class_ref 129685 // Katip
 name ""  xyz 508 277 2000
note 130069 "Setup procedure"
  xyzwh 60 50 2000 151 49
linkcanvas 128277
  from ref 128149 z 2001 to ref 128021
dirscanvas 128405 z 1000 linkcanvas_ref 128277
  
  forward_label "1 setup()" xyz 174 132 3000
linkcanvas 128661
  from ref 128149 z 2001 to ref 128533
dirscanvas 128789 z 1000 linkcanvas_ref 128661
  
  forward_label "2 setup()" xyz 239 175 3000
linkcanvas 129301
  from ref 128533 z 2001 to ref 128917
dirscanvas 129429 z 1000 linkcanvas_ref 129301
  
  forward_label "3 setup()" xyz 399 132 3000
linkcanvas 129557
  from ref 128533 z 2001 to ref 129045
dirscanvas 129685 z 1000 linkcanvas_ref 129557
  
  forward_label "4 setup()" xyz 441 174 3000
linkcanvas 129813
  from ref 128533 z 2001 to ref 129173
dirscanvas 129941 z 1000 linkcanvas_ref 129813
  
  forward_label "5 setup()" xyz 450 215 3000
msgs
  msg operation_ref 157845 // "setup()"
    forward ranks 1 "1" dirscanvas_ref 128405
    no_msg
  msg operation_ref 144149 // "setup(in c : Configuration)"
    forward ranks 2 "2" dirscanvas_ref 128789
    msgs
      msg operation_ref 144149 // "setup(in c : Configuration)"
	forward ranks 3 "2.1" dirscanvas_ref 129429
	no_msg
      msg operation_ref 144149 // "setup(in c : Configuration)"
	forward ranks 4 "2.2" dirscanvas_ref 129685
	no_msg
      msg operation_ref 144149 // "setup(in c : Configuration)"
	forward ranks 5 "2.3" dirscanvas_ref 129941
	no_msg
    msgsend
msgsend
end
