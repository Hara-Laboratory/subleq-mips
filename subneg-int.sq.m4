@add Rd,Rs,Rt
Rs Z; // Z <- -Rs, it assumes Z = 0
Rt Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

@mult Lo,Rs,Rt // incomplete
Dec T1;
Z Rs Lsns;                   // if Rs < 0 then Lns else Lps
Lsps:Rs Z; Z T2; Z Z; 		// T2 <- Rs  = abs(Rs)
Z Inc Lct;					// goto Lct
Lsns:Rs T2; Dec T1;          // T2 <- T2-Rs = abs(Rs), T1 <- -1  
Lct:Z Rt Ltns;                   // if Rs < 0 then Lns else Lps
Ltps:Rt Z; Z T3; Z Z; 		// T2 <- 2*Rs  = abs(Rs)
Z Inc Lmultu;				// goto Lmultu
Ltns:Rt T3; Dec T1;          // T2 <- T2-Rs = abs(Rs), T1 <- -1  
Lmultu:Lo;                  // it's ok because Rt and lo are not aliased.
$(@@multu Lo,T2,T3,Linv);    // lo <- abs(Rs) * (-Rt)
Linv:Z T1 Linvloop;          // if Rs < 0 (i.e. T1 < 0) then Linvloop else next
Lend:T1; T2; Z Z End;
Linvloop:T2; Lo T2; T3; T2 T3; Lo; T3 Lo; Inc T1 Linvloop; Z Inc Lend;

@@multu Ad,Ax,Ay,Aend // internal subroutine macro: Ad <- Ax * (-Ay) + Ad; assumes Ax >= 0; modifies Ax, Ad
Lloop:Dec Ax Aend;
Ay Ad Lloop;

@shl1 Rd,Rs
Rs Z; // Z <- -Rs, it assumes Z = 0
Rs Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

@floor2pow R1, R2, A
	Z Inc End;
