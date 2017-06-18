Nsize = 10;
Xa = 0.5; Xb = 1;
dx = (Xb - Xa)/(Nsize - 1);
Func[x_] := Exp[-(x - 3/4)^2];
DiscFunc = Table[{i*dx + Xa, Func[i*dx + Xa]}, {i, 0, Nsize - 1}];
P[x_] := InterpolatingPolynomial[DiscFunc, x];
Plot[{P[x], Func[x]}, {x, Xa, Xb}, PlotStyle -> {PointSize[1.5], Dashed}, PlotPoints -> {Nsize, 0}]