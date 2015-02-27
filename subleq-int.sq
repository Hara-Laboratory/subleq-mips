@add Rd,Rs,Rt
Rs Z; // Z <- -Rs, it assumes Z = 0
Rt Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

@mult Lo,Rs,Rt // incomplete
Z Rs Lsns;                   // if Rs < 0 then Lns else Lps
Lsps:Rs Z; Z T2; Z Z Lct; 		// T2 <- Rs  = abs(Rs)
Lsns:Rs T2; Dec T1;          // T2 <- T2-Rs = abs(Rs), T1 <- -1  
Lct:Z Rt Ltns;                   // if Rs < 0 then Lns else Lps
Ltps:Rt Z; Z T3; Z Z Lmultu; 		// T2 <- 2*Rs  = abs(Rs)
Ltns:Rt T3; Dec T1;          // T2 <- T2-Rs = abs(Rs), T1 <- -1  
Lmultu:Lo;                  // it's ok because Rt and lo are not aliased.
$(@@multuSub Lo,T2,T3,Linv);    // lo <- abs(Rs) * (-Rt)
Linv:Z T1 Linvloop;          // if Rs <= 0 (i.e. T1 < 0) then Linvloop else next
Lend:T1; T2; Z Z End;
Linvloop:T2; Lo T2; T3; T2 T3; Lo; T3 Lo; Inc T1 Linvloop; Z Z Lend;

@multu Rd,Rx,Ry // internal subroutine macro: Ad <- Ax * (-Ay) + Ad; assumes Ax >= 0, Ay >= 0; modifies Ax, Ad
Rd;
$(@@multuSub Rd, Rx, Ry, End);

@@multuSub Ad,Ax,Ay,Aend // internal subroutine macro: Ad <- Ax * (-Ay) + Ad; assumes Ax >= 0, Ay >= 0; modifies Ax, Ad
Lloop:Z Ax Aend;
Dec Ax;
Ay Ad Lloop;

@shl1 Rd,Rs
Rs Z; // Z <- -Rs, it assumes Z = 0
Rs Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

@bne Rs, Rt, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rt T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

@bnea Rs, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rs T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

@ Rs, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rs T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

