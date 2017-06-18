Nsize = 10;
Xa = Pi - 0.01; Xb = Pi + 0.01;
dx = (Xb - Xa)/(Nsize - 1);
Func[x_] = 3*x^2 + Log[(Pi - x)^2]/Pi^4 + 1
DiscFunc = Table[{i*dx + Xa, Func[i*dx + Xa]}, {i, 0, Nsize - 1}];
P := Interpolation[DiscFunc, Method -> "Spline"]
Plot[{P[x], Func[x]}, {x, Xa, Xb}]