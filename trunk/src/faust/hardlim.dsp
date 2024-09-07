declare id "hardlim";
declare name "?limiter";
//declare category "Guitar Effects";

import("stdfaust.lib");
import("guitarix.lib");
rd = library("reducemaps.lib");


compressor_stereo(ratio,thresh,att,rel,x,y) = cgm*x, cgm*y with {
  cgm = compression_gain_mono(ratio,thresh,att,rel,max(abs(x),abs(y)));
};

compression_gain_mono(ratio,thresh,att,rel) =
  an.amp_follower_ar(att,rel) : ba.linear2db  : outminusindb(ratio,thresh) :
  kneesmooth(att) : ba.db2linear : vmeter1
with {
  // kneesmooth(att) installs a "knee" in the dynamic-range compression,
  // where knee smoothness is set equal to half that of the compression-attack.
  // A general 'knee' parameter could be used instead of tying it to att/2:
  kneesmooth(att)  = si.smooth(ba.tau2pole(att/2.0));
  // compression gain in dB:
   outminusindb(ratio,thresh,level) = max(level-thresh,0.0) * (1.0/float(ratio)-1.0) ;
  // Note: "float(ratio)" REQUIRED when ratio is an integer > 1!
  // compression meter indicate when the limiter kicks in
  vmeter1(x) = attach(x, envelop(1.0-x) : vbargraph("v1[nomidi][tooltip:Rack output Limiter]", 0.0, 1.0));
  envelop    = abs : max ~ (1.0/ma.SR) : rd.maxn(1024); 
};

softclip(th,x) = softsat(preclip(x)) with {
  // The softsat function will map [-cth,cth] to [-1,1], but outside of that input range
  // it is not well behaved.  So, hard clip to the valid input range first.
  preclip(x) = aa.clip(-cth,cth,x);
  // Defines a transfer function with linearly decaying derivative ouside of [-th,th]
  softsat(x) = ba.if(ax<=th,x,ma.signum(x)*((cth-(ax+th)/2.0)*(ax-th)/2.0/(1.0-th) + th));
  cth = 2-th;                
  ax=abs(x);
};
softclip_stereo(th,x,y) = softclip(th,x),softclip(th,y);

// ::: Clipper ::
// Leave room for an occasional peak overshooting the limiter
lim_ceiling = ba.db2linear(-3.0);   // below 0.0dB
// This is where soft clipping will start
clip_ceiling = ba.db2linear(-3.0);  // below 0.0dB

                             
lim = compressor_stereo(20,lim_ceiling,0.0008,0.5) : softclip_stereo(clip_ceiling) ;

process = lim;

