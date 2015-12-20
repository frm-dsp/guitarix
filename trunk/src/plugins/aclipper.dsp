declare id 		"aclipper";
declare name            "Rat";
declare category        "Distortion";


rat_in = component("filter.lib").iir((b0/a0,b1/a0,b2/a0),(a1/a0,a2/a0)) with {
    SR = component("math.lib").SR;
    s = 0.993;
    fs = float(SR);

    b0 = 1.82432723786905e-5*fs;

    b1 = 0.000829239653576842;

    b2 = -1.82432723786905e-5*fs;

    a0 = fs*(3.64865447573811e-11*fs + 1.9073341271921e-5) + 0.000414619826788421;

    a1 = -7.29730895147621e-11*pow(fs,2) + 0.000829239653576842;

    a2 = fs*(3.64865447573811e-11*fs - 1.9073341271921e-5) + 0.000414619826788421;
};

rat_out = component("filter.lib").iir((b0/a0,b1/a0),(a1/a0)) with {
    SR = component("math.lib").SR;
    s = 0.993;
    fs = float(SR);

    b0 = 2.08332871602678e-5*fs;

    b1 = -2.08332871602678e-5*fs;

    a0 = 2.08332871602678e-5*fs + 2.21630714470934e-6;

    a1 = -2.08332871602678e-5*fs + 2.21630714470934e-6;
};

rat_tone =  component("filter.lib").iir((b0/a0,b1/a0),(a1/a0)) with {
    SR = component("math.lib").SR;
    s = 0.993;
    fs = float(SR);

    Tone = 1.0 - vslider("tone[name:Tone]", 0.5, 0, 1, 0.01) : component("filter.lib").smooth(s);
    
    b0 = 0.0593824228028504;

    b1 = 0.0593824228028504;

    a0 = -3.91923990498812e-5*Tone*fs + 3.91923990498812e-5*fs + 0.0593824228028504;

    a1 = 3.91923990498812e-5*Tone*fs - 3.91923990498812e-5*fs + 0.0593824228028504;
};

process = rat_in : X3 : rat_out : *(fulldrive): aclip : rat_tone : *(gain)  with {
    SR = component("math.lib").SR;
    s = 0.993;
    R1 = 4700;
    drive =  hslider("drive[name:Drive]", 0.5, 0, 1, 0.01);
    fulldrive = 1.0 + max(0.0, drive-0.25);
    R2 = 1000 + 100000 * drive;
    C = 0.047 * 1e-6;
    a1 = (R1 + R2) * C * 2 * SR;
    a2 = R1 * C * 2 * SR;
    B0 = (1 + a1) / (1 + a2);
    B1 = (1 - a1) / (1 + a2);
    A1 = (1 - a2) / (1 + a2);
    X1 = _<:_,(rat_drive):>_;
    X2 = component("filter.lib").tf1(B0, B1, A1);
    opamp = ffunction(float opamp(float), "clipping.h", "");
    X3 = _ <: _ - opamp(X2-_) :> _ ;
    asymclip = ffunction(float asymhardclip2(float), "clipping.h", "");
    aclip = asymclip(_);
    gain = hslider("level[name:Level]", -2, -20, 12, 0.1) : component("music.lib").db2linear : component("filter.lib").smooth(s);
};
