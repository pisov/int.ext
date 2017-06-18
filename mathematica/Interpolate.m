Nsize = 10;
Xa = 0.5; Xb = 1;
dx = (Xb - Xa)/(Nsize - 1);
Func[x_] := Exp[-(x - 3/4)^2];
DiscFunc = Table[{i*dx + Xa, Func[i*dx + Xa]}, {i, 0, Nsize - 1}];
P := Interpolation[DiscFunc, Method -> "Spline"]
Plot[P[x], {x, Xa, Xb}]