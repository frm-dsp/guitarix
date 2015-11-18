// generated from file '../src/faust/gxamp9.dsp' by dsp2cc:
// Code generated with Faust 0.9.65 (http://faust.grame.fr)

#include "valve.h"

namespace gxamp9 {

class Dsp: public PluginDef {
private:
	int fSamplingFreq;
	FAUSTFLOAT 	fslider0;
	FAUSTFLOAT	*fslider0_;
	double 	fRec0[2];
	FAUSTFLOAT 	fslider1;
	FAUSTFLOAT	*fslider1_;
	double 	fRec12[2];
	int 	iConst0;
	double 	fConst1;
	double 	fConst2;
	double 	fConst3;
	double 	fConst4;
	double 	fConst5;
	double 	fConst6;
	double 	fConst7;
	double 	fConst8;
	double 	fConst9;
	double 	fConst10;
	double 	fConst11;
	double 	fConst12;
	double 	fConst13;
	double 	fConst14;
	double 	fConst15;
	double 	fConst16;
	double 	fConst17;
	double 	fConst18;
	double 	fConst19;
	double 	fConst20;
	double 	fConst21;
	double 	fConst22;
	double 	fConst23;
	double 	fConst24;
	double 	fConst25;
	double 	fConst26;
	double 	fRec29[2];
	int 	IOTA;
	double 	fVec0[32768];
	int 	iConst27;
	double 	fRec28[2];
	double 	fConst28;
	double 	fRec27[2];
	FAUSTFLOAT 	fslider2;
	FAUSTFLOAT	*fslider2_;
	double 	fRec30[2];
	double 	fRec26[3];
	double 	fVec1[2];
	double 	fConst29;
	double 	fConst30;
	double 	fConst31;
	double 	fConst32;
	double 	fConst33;
	double 	fRec25[2];
	double 	fConst34;
	double 	fConst35;
	double 	fConst36;
	double 	fRec24[2];
	double 	fRec23[3];
	double 	fVec2[2];
	double 	fConst37;
	double 	fConst38;
	double 	fConst39;
	double 	fRec22[2];
	double 	fRec21[3];
	double 	fVec3[2];
	double 	fConst40;
	double 	fConst41;
	double 	fConst42;
	double 	fRec20[2];
	double 	fRec19[3];
	FAUSTFLOAT 	fslider3;
	FAUSTFLOAT	*fslider3_;
	double 	fConst43;
	double 	fConst44;
	double 	fConst45;
	double 	fRec33[2];
	double 	fRec32[3];
	double 	fConst46;
	double 	fRec31[3];
	double 	fConst47;
	double 	fConst48;
	double 	fConst49;
	double 	fRec37[2];
	double 	fRec36[3];
	double 	fConst50;
	double 	fRec35[3];
	double 	fRec34[3];
	double 	fConst51;
	double 	fRec39[2];
	double 	fRec38[3];
	double 	fConst52;
	double 	fVec4[2];
	double 	fConst53;
	double 	fConst54;
	double 	fConst55;
	double 	fConst56;
	double 	fRec18[2];
	double 	fRec17[3];
	double 	fVec5[2];
	double 	fRec16[2];
	double 	fConst57;
	double 	fConst58;
	double 	fConst59;
	double 	fConst60;
	double 	fRec40[2];
	double 	fRec15[3];
	double 	fConst61;
	double 	fRec14[2];
	double 	fRec44[3];
	double 	fVec6[2];
	double 	fRec43[2];
	double 	fConst62;
	double 	fConst63;
	double 	fConst64;
	double 	fConst65;
	double 	fRec45[2];
	double 	fRec42[3];
	double 	fRec41[2];
	double 	fRec13[2];
	double 	fVec7[2];
	double 	fRec11[2];
	double 	fRec10[3];
	double 	fVec8[2];
	double 	fRec9[2];
	double 	fRec46[2];
	double 	fRec8[3];
	double 	fRec7[2];
	double 	fRec50[3];
	double 	fVec9[2];
	double 	fRec49[2];
	double 	fConst66;
	double 	fConst67;
	double 	fConst68;
	double 	fConst69;
	double 	fRec51[2];
	double 	fRec48[3];
	double 	fRec47[2];
	double 	fRec6[2];
	double 	fRec5[2];
	double 	fRec4[3];
	double 	fVec10[2];
	double 	fRec3[2];
	double 	fRec52[2];
	double 	fRec2[3];
	double 	fRec1[2];
	void clear_state_f();
	void init(unsigned int samplingFreq);
	void compute(int count, FAUSTFLOAT *input0, FAUSTFLOAT *output0);
	int register_par(const ParamReg& reg);

	static void clear_state_f_static(PluginDef*);
	static void init_static(unsigned int samplingFreq, PluginDef*);
	static void compute_static(int count, FAUSTFLOAT *input0, FAUSTFLOAT *output0, PluginDef*);
	static int register_params_static(const ParamReg& reg);
	static void del_instance(PluginDef *p);
public:
	Dsp();
	~Dsp();
};



Dsp::Dsp()
	: PluginDef() {
	version = PLUGINDEF_VERSION;
	flags = 0;
	id = "12ax7 feedback";
	name = N_("12ax7 feedback");
	groups = 0;
	description = ""; // description (tooltip)
	category = "";       // category
	shortname = "";     // shortname
	mono_audio = compute_static;
	stereo_audio = 0;
	set_samplerate = init_static;
	activate_plugin = 0;
	register_params = register_params_static;
	load_ui = 0;
	clear_state = clear_state_f_static;
	delete_instance = del_instance;
}

Dsp::~Dsp() {
}

inline void Dsp::clear_state_f()
{
	for (int i=0; i<2; i++) fRec0[i] = 0;
	for (int i=0; i<2; i++) fRec12[i] = 0;
	for (int i=0; i<2; i++) fRec29[i] = 0;
	for (int i=0; i<32768; i++) fVec0[i] = 0;
	for (int i=0; i<2; i++) fRec28[i] = 0;
	for (int i=0; i<2; i++) fRec27[i] = 0;
	for (int i=0; i<2; i++) fRec30[i] = 0;
	for (int i=0; i<3; i++) fRec26[i] = 0;
	for (int i=0; i<2; i++) fVec1[i] = 0;
	for (int i=0; i<2; i++) fRec25[i] = 0;
	for (int i=0; i<2; i++) fRec24[i] = 0;
	for (int i=0; i<3; i++) fRec23[i] = 0;
	for (int i=0; i<2; i++) fVec2[i] = 0;
	for (int i=0; i<2; i++) fRec22[i] = 0;
	for (int i=0; i<3; i++) fRec21[i] = 0;
	for (int i=0; i<2; i++) fVec3[i] = 0;
	for (int i=0; i<2; i++) fRec20[i] = 0;
	for (int i=0; i<3; i++) fRec19[i] = 0;
	for (int i=0; i<2; i++) fRec33[i] = 0;
	for (int i=0; i<3; i++) fRec32[i] = 0;
	for (int i=0; i<3; i++) fRec31[i] = 0;
	for (int i=0; i<2; i++) fRec37[i] = 0;
	for (int i=0; i<3; i++) fRec36[i] = 0;
	for (int i=0; i<3; i++) fRec35[i] = 0;
	for (int i=0; i<3; i++) fRec34[i] = 0;
	for (int i=0; i<2; i++) fRec39[i] = 0;
	for (int i=0; i<3; i++) fRec38[i] = 0;
	for (int i=0; i<2; i++) fVec4[i] = 0;
	for (int i=0; i<2; i++) fRec18[i] = 0;
	for (int i=0; i<3; i++) fRec17[i] = 0;
	for (int i=0; i<2; i++) fVec5[i] = 0;
	for (int i=0; i<2; i++) fRec16[i] = 0;
	for (int i=0; i<2; i++) fRec40[i] = 0;
	for (int i=0; i<3; i++) fRec15[i] = 0;
	for (int i=0; i<2; i++) fRec14[i] = 0;
	for (int i=0; i<3; i++) fRec44[i] = 0;
	for (int i=0; i<2; i++) fVec6[i] = 0;
	for (int i=0; i<2; i++) fRec43[i] = 0;
	for (int i=0; i<2; i++) fRec45[i] = 0;
	for (int i=0; i<3; i++) fRec42[i] = 0;
	for (int i=0; i<2; i++) fRec41[i] = 0;
	for (int i=0; i<2; i++) fRec13[i] = 0;
	for (int i=0; i<2; i++) fVec7[i] = 0;
	for (int i=0; i<2; i++) fRec11[i] = 0;
	for (int i=0; i<3; i++) fRec10[i] = 0;
	for (int i=0; i<2; i++) fVec8[i] = 0;
	for (int i=0; i<2; i++) fRec9[i] = 0;
	for (int i=0; i<2; i++) fRec46[i] = 0;
	for (int i=0; i<3; i++) fRec8[i] = 0;
	for (int i=0; i<2; i++) fRec7[i] = 0;
	for (int i=0; i<3; i++) fRec50[i] = 0;
	for (int i=0; i<2; i++) fVec9[i] = 0;
	for (int i=0; i<2; i++) fRec49[i] = 0;
	for (int i=0; i<2; i++) fRec51[i] = 0;
	for (int i=0; i<3; i++) fRec48[i] = 0;
	for (int i=0; i<2; i++) fRec47[i] = 0;
	for (int i=0; i<2; i++) fRec6[i] = 0;
	for (int i=0; i<2; i++) fRec5[i] = 0;
	for (int i=0; i<3; i++) fRec4[i] = 0;
	for (int i=0; i<2; i++) fVec10[i] = 0;
	for (int i=0; i<2; i++) fRec3[i] = 0;
	for (int i=0; i<2; i++) fRec52[i] = 0;
	for (int i=0; i<3; i++) fRec2[i] = 0;
	for (int i=0; i<2; i++) fRec1[i] = 0;
}

void Dsp::clear_state_f_static(PluginDef *p)
{
	static_cast<Dsp*>(p)->clear_state_f();
}

inline void Dsp::init(unsigned int samplingFreq)
{
	fSamplingFreq = samplingFreq;
	iConst0 = min(192000, max(1, fSamplingFreq));
	fConst1 = tan((942.4777960769379 / double(iConst0)));
	fConst2 = (1.0 / faustpower<2>(fConst1));
	fConst3 = (2 * (1 - fConst2));
	fConst4 = (1.0 / fConst1);
	fConst5 = (1 + ((fConst4 - 1.0000000000000004) / fConst1));
	fConst6 = (1.0 / (1 + ((fConst4 + 1.0000000000000004) / fConst1)));
	fConst7 = tan((3769.9111843077517 / double(iConst0)));
	fConst8 = (1.0 / faustpower<2>(fConst7));
	fConst9 = (2 * (1 - fConst8));
	fConst10 = (1.0 / fConst7);
	fConst11 = (1 + ((fConst10 - 1.0000000000000004) / fConst7));
	fConst12 = (1 + ((fConst10 + 1.0000000000000004) / fConst7));
	fConst13 = (1.0 / fConst12);
	fConst14 = tan((10053.096491487338 / double(iConst0)));
	fConst15 = (1.0 / faustpower<2>(fConst14));
	fConst16 = (2 * (1 - fConst15));
	fConst17 = (1.0 / fConst14);
	fConst18 = (1 + ((fConst17 - 1.0000000000000004) / fConst14));
	fConst19 = (1 + ((1.0000000000000004 + fConst17) / fConst14));
	fConst20 = (1.0 / fConst19);
	fConst21 = tan((47123.8898038469 / double(iConst0)));
	fConst22 = (2 * (1 - (1.0 / faustpower<2>(fConst21))));
	fConst23 = (1.0 / fConst21);
	fConst24 = (1 + ((fConst23 - 1.414213562373095) / fConst21));
	fConst25 = (1 + ((1.414213562373095 + fConst23) / fConst21));
	fConst26 = (1.0 / fConst25);
	IOTA = 0;
	iConst27 = int((int((0.1111111111111111 * iConst0)) & 65535));
	fConst28 = (0.009000000000000008 / double(iConst0));
	fConst29 = (1.0 / tan((97.38937226128358 / double(iConst0))));
	fConst30 = (0 - fConst29);
	fConst31 = (1 + fConst29);
	fConst32 = (1.0 / (fConst31 * fConst25));
	fConst33 = (0 - ((1 - fConst29) / fConst31));
	fConst34 = (1 + fConst17);
	fConst35 = (1.0 / fConst34);
	fConst36 = (0 - ((1 - fConst17) / fConst34));
	fConst37 = (1 + fConst10);
	fConst38 = (1.0 / (fConst37 * fConst19));
	fConst39 = (0 - ((1 - fConst10) / fConst37));
	fConst40 = (1 + fConst4);
	fConst41 = (1.0 / (fConst40 * fConst12));
	fConst42 = (0 - ((1 - fConst4) / fConst40));
	fConst43 = (1 + ((fConst4 - 1.0) / fConst1));
	fConst44 = (1.0 / (1 + ((1.0 + fConst4) / fConst1)));
	fConst45 = (0 - fConst10);
	fConst46 = (2 * (0 - fConst8));
	fConst47 = (1 + ((fConst10 - 1.0) / fConst7));
	fConst48 = (1.0 / (1 + ((1.0 + fConst10) / fConst7)));
	fConst49 = (0 - fConst17);
	fConst50 = (2 * (0 - fConst15));
	fConst51 = (0 - fConst4);
	fConst52 = (2 * (0 - fConst2));
	fConst53 = (1.0 / tan((20517.741620594938 / double(iConst0))));
	fConst54 = (1 + fConst53);
	fConst55 = (1.0 / fConst54);
	fConst56 = (0 - ((1 - fConst53) / fConst54));
	fConst57 = (1.0 / tan((270.1769682087222 / double(iConst0))));
	fConst58 = (1 + fConst57);
	fConst59 = (0.027 / fConst58);
	fConst60 = (0 - ((1 - fConst57) / fConst58));
	fConst61 = (0.025 / fConst31);
	fConst62 = (1.0 / tan((414.6902302738527 / double(iConst0))));
	fConst63 = (1 + fConst62);
	fConst64 = (0.015 / fConst63);
	fConst65 = (0 - ((1 - fConst62) / fConst63));
	fConst66 = (1.0 / tan((609.4689747964198 / double(iConst0))));
	fConst67 = (1 + fConst66);
	fConst68 = (0.0082 / fConst67);
	fConst69 = (0 - ((1 - fConst66) / fConst67));
	clear_state_f();
}

void Dsp::init_static(unsigned int samplingFreq, PluginDef *p)
{
	static_cast<Dsp*>(p)->init(samplingFreq);
}

void always_inline Dsp::compute(int count, FAUSTFLOAT *input0, FAUSTFLOAT *output0)
{
#define fslider0 (*fslider0_)
#define fslider1 (*fslider1_)
#define fslider2 (*fslider2_)
#define fslider3 (*fslider3_)
	double 	fSlow0 = (0.0010000000000000009 * pow(10,(0.05 * double(fslider0))));
	double 	fSlow1 = (0.0010000000000000009 * pow(10,(0.05 * double(fslider1))));
	double 	fSlow2 = (1.000000000000001e-05 * double(fslider2));
	double 	fSlow3 = double(fslider3);
	double 	fSlow4 = (fConst6 * pow(1e+01,(0.9 * fSlow3)));
	double 	fSlow5 = pow(1e+01,(1.2 * fSlow3));
	double 	fSlow6 = pow(1e+01,(0.8 * fSlow3));
	double 	fSlow7 = pow(1e+01,(2 * fSlow3));
	double 	fSlow8 = (fConst6 * fSlow7);
	double 	fSlow9 = (2 * (fSlow3 - 0.5));
	double 	fSlow10 = (1 - max((double)0, (0 - fSlow9)));
	double 	fSlow11 = (0.024937655860349125 * (1 - max((double)0, fSlow9)));
	double 	fSlow12 = (1.25 * fSlow3);
	for (int i=0; i<count; i++) {
		fRec0[0] = ((0.999 * fRec0[1]) + fSlow0);
		double fTemp0 = (2.0 * fRec4[1]);
		double fTemp1 = (2.0 * fRec10[1]);
		fRec12[0] = ((0.999 * fRec12[1]) + fSlow1);
		double fTemp2 = (2.0 * fRec17[1]);
		double fTemp3 = (double)input0[i];
		double fTemp4 = fabs(fTemp3);
		fRec29[0] = ((0.9999 * fRec29[1]) + (9.999999999998899e-05 * fTemp4));
		double fTemp5 = max(fRec29[0], fTemp4);
		fVec0[IOTA&32767] = fTemp5;
		fRec28[0] = ((fVec0[IOTA&32767] + fRec28[1]) - fVec0[(IOTA-iConst27)&32767]);
		fRec27[0] = ((0.999 * fRec27[1]) + (fConst28 * fRec28[0]));
		double fTemp6 = max((double)-1, min(-0.01, (fVec0[IOTA&32767] - (1.0 + (1.02 * fRec27[0])))));
		double fTemp7 = (0 - fTemp6);
		double fTemp8 = (40.1 * fTemp7);
		double fTemp9 = max((double)-600, fTemp8);
		double fTemp10 = (0 - fTemp9);
		double fTemp11 = (fTemp3 - fTemp6);
		double fTemp12 = (40.1 * fTemp11);
		double fTemp13 = max((double)-600, fTemp12);
		double fTemp14 = (0 - fTemp13);
		double fTemp15 = (((int((fabs(fTemp12) > 0.0001)))?((int((fTemp13 < -50)))?(fTemp14 * exp(fTemp13)):(fTemp13 / (1 - exp(fTemp14)))):(1 + (fTemp11 * (20.05 + (134.00083333333336 * fTemp11))))) - ((int((fabs(fTemp8) > 0.0001)))?((int((fTemp9 < -50)))?(fTemp10 * exp(fTemp9)):(fTemp9 / (1 - exp(fTemp10)))):(1 + (fTemp7 * (20.05 + (134.00083333333336 * fTemp7))))));
		fRec30[0] = (fSlow2 + (0.999 * fRec30[1]));
		double fTemp16 = (0.024937655860349125 * (fRec30[0] * fTemp15));
		fRec26[0] = (fTemp16 - (fConst26 * ((fConst24 * fRec26[2]) + (fConst22 * fRec26[1]))));
		double fTemp17 = (fRec26[2] + (fRec26[0] + (2 * fRec26[1])));
		fVec1[0] = fTemp17;
		fRec25[0] = ((fConst33 * fRec25[1]) + (fConst32 * ((fConst29 * fVec1[0]) + (fConst30 * fVec1[1]))));
		fRec24[0] = ((fConst36 * fRec24[1]) + (fConst35 * (fRec25[0] + fRec25[1])));
		fRec23[0] = (fRec24[0] - (fConst20 * ((fConst18 * fRec23[2]) + (fConst16 * fRec23[1]))));
		double fTemp18 = (fRec23[2] + (fRec23[0] + (2 * fRec23[1])));
		fVec2[0] = fTemp18;
		fRec22[0] = ((fConst39 * fRec22[1]) + (fConst38 * (fVec2[0] + fVec2[1])));
		fRec21[0] = (fRec22[0] - (fConst13 * ((fConst11 * fRec21[2]) + (fConst9 * fRec21[1]))));
		double fTemp19 = (fRec21[2] + (fRec21[0] + (2 * fRec21[1])));
		fVec3[0] = fTemp19;
		fRec20[0] = ((fConst42 * fRec20[1]) + (fConst41 * (fVec3[0] + fVec3[1])));
		fRec19[0] = (fRec20[0] - (fConst6 * ((fConst5 * fRec19[2]) + (fConst3 * fRec19[1]))));
		double fTemp20 = max((double)-1, min((double)1, (fSlow4 * (fRec19[2] + (fRec19[0] + (2 * fRec19[1]))))));
		double fTemp21 = (fConst3 * fRec31[1]);
		fRec33[0] = ((fConst39 * fRec33[1]) + (fConst38 * ((fConst10 * fVec2[0]) + (fConst45 * fVec2[1]))));
		fRec32[0] = (fRec33[0] - (fConst13 * ((fConst11 * fRec32[2]) + (fConst9 * fRec32[1]))));
		fRec31[0] = ((fConst13 * (((fConst8 * fRec32[0]) + (fConst46 * fRec32[1])) + (fConst8 * fRec32[2]))) - (fConst44 * ((fConst43 * fRec31[2]) + fTemp21)));
		double fTemp22 = max((double)-1, min((double)1, (fSlow5 * (fRec31[2] + (fConst44 * (fTemp21 + (fConst43 * fRec31[0])))))));
		double fTemp23 = (fConst3 * fRec34[1]);
		double fTemp24 = (fConst9 * fRec35[1]);
		fRec37[0] = ((fConst36 * fRec37[1]) + (fConst35 * ((fConst17 * fRec25[0]) + (fConst49 * fRec25[1]))));
		fRec36[0] = (fRec37[0] - (fConst20 * ((fConst18 * fRec36[2]) + (fConst16 * fRec36[1]))));
		fRec35[0] = ((fConst20 * (((fConst15 * fRec36[0]) + (fConst50 * fRec36[1])) + (fConst15 * fRec36[2]))) - (fConst48 * ((fConst47 * fRec35[2]) + fTemp24)));
		fRec34[0] = ((fRec35[2] + (fConst48 * (fTemp24 + (fConst47 * fRec35[0])))) - (fConst44 * ((fConst43 * fRec34[2]) + fTemp23)));
		double fTemp25 = max((double)-1, min((double)1, (fSlow6 * (fRec34[2] + (fConst44 * (fTemp23 + (fConst43 * fRec34[0])))))));
		fRec39[0] = ((fConst42 * fRec39[1]) + (fConst41 * ((fConst4 * fVec3[0]) + (fConst51 * fVec3[1]))));
		fRec38[0] = (fRec39[0] - (fConst6 * ((fConst5 * fRec38[2]) + (fConst3 * fRec38[1]))));
		double fTemp26 = max((double)-1, min((double)1, (fSlow8 * (((fConst2 * fRec38[0]) + (fConst52 * fRec38[1])) + (fConst2 * fRec38[2])))));
		double fTemp27 = ((1.584893192 * ((fTemp26 * (1 - (0.3333333333333333 * faustpower<2>(fTemp26)))) + ((fTemp25 * (1 - (0.3333333333333333 * faustpower<2>(fTemp25)))) + (0.8413951417869425 * (fTemp22 * (1 - (0.3333333333333333 * faustpower<2>(fTemp22)))))))) + (1.2589412 * (fTemp20 * (1 - (0.3333333333333333 * faustpower<2>(fTemp20))))));
		fVec4[0] = fTemp27;
		fRec18[0] = ((fConst56 * fRec18[1]) + (fConst55 * (fVec4[0] + fVec4[1])));
		double fTemp28 = max((double)-1, min((double)1, (fSlow7 * max(-0.7, min(0.7, fTemp16)))));
		double fTemp29 = (1 - fRec30[0]);
		fRec17[0] = (((fTemp15 * ((fTemp29 * (0.024937655860349125 + (fSlow12 * ((0.0997506234413965 - (0.0997506234413965 * fabs((0.024937655860349125 * (fTemp29 * fTemp15))))) - 0.024937655860349125)))) + (fSlow11 * fRec30[0]))) + ((0.5 * (fTemp28 * (1 - (0.3333333333333333 * faustpower<2>(fTemp28))))) + (fSlow10 * fRec18[0]))) - (fTemp2 + fRec17[2]));
		double fTemp30 = (fRec17[2] + (fRec17[0] + fTemp2));
		fVec5[0] = fTemp30;
		fRec16[0] = ((1.0000000000000002 * (fVec5[0] + fVec5[1])) - (0.9999999999999998 * fRec16[1]));
		fRec40[0] = ((fConst60 * fRec40[1]) + (fConst59 * (fRec15[1] + fRec15[2])));
		fRec15[0] = (Ftube(TUBE_TABLE_12AX7_68k, ((fRec40[0] + fRec16[0]) - 1.581656)) - 191.42014814814814);
		fRec14[0] = ((fConst33 * fRec14[1]) + (fConst61 * ((fConst29 * fRec15[0]) + (fConst30 * fRec15[1]))));
		double fTemp31 = (2.0 * fRec44[1]);
		fRec44[0] = (fRec13[1] - (fTemp31 + fRec44[2]));
		double fTemp32 = (fRec44[2] + (fRec44[0] + fTemp31));
		fVec6[0] = fTemp32;
		fRec43[0] = ((1.0000000000000002 * (fVec6[0] + fVec6[1])) - (0.9999999999999998 * fRec43[1]));
		fRec45[0] = ((fConst65 * fRec45[1]) + (fConst64 * (fRec42[1] + fRec42[2])));
		fRec42[0] = (Ftube(TUBE_TABLE_12AX7_250k, ((fRec45[0] + fRec43[0]) - 1.204285)) - 169.71433333333334);
		fRec41[0] = ((fConst33 * fRec41[1]) + (fConst61 * ((fConst29 * fRec42[0]) + (fConst30 * fRec42[1]))));
		fRec13[0] = ((0.6 * fRec41[0]) - fRec14[0]);
		double fTemp33 = (fRec13[0] * fRec12[0]);
		fVec7[0] = fTemp33;
		fRec11[0] = ((fConst56 * fRec11[1]) + (fConst55 * (fVec7[0] + fVec7[1])));
		fRec10[0] = (fRec11[0] - (fTemp1 + fRec10[2]));
		double fTemp34 = (fRec10[2] + (fRec10[0] + fTemp1));
		fVec8[0] = fTemp34;
		fRec9[0] = ((1.0000000000000002 * (fVec8[0] + fVec8[1])) - (0.9999999999999998 * fRec9[1]));
		fRec46[0] = ((fConst65 * fRec46[1]) + (fConst64 * (fRec8[1] + fRec8[2])));
		fRec8[0] = (Ftube(TUBE_TABLE_12AX7_250k, ((fRec46[0] + fRec9[0]) - 1.204285)) - 169.71433333333334);
		fRec7[0] = ((fConst33 * fRec7[1]) + (fConst61 * ((fConst29 * fRec8[0]) + (fConst30 * fRec8[1]))));
		double fTemp35 = (2.0 * fRec50[1]);
		fRec50[0] = (fRec6[1] - (fTemp35 + fRec50[2]));
		double fTemp36 = (fRec50[2] + (fRec50[0] + fTemp35));
		fVec9[0] = fTemp36;
		fRec49[0] = ((1.0000000000000002 * (fVec9[0] + fVec9[1])) - (0.9999999999999998 * fRec49[1]));
		fRec51[0] = ((fConst69 * fRec51[1]) + (fConst68 * (fRec48[1] + fRec48[2])));
		fRec48[0] = (Ftube(TUBE_TABLE_12AX7_250k, ((fRec51[0] + fRec49[0]) - 0.840702)) - 147.47536585365856);
		fRec47[0] = ((fConst33 * fRec47[1]) + (fConst61 * ((fConst29 * fRec48[0]) + (fConst30 * fRec48[1]))));
		fRec6[0] = ((0.6 * fRec47[0]) + fRec7[0]);
		fRec5[0] = ((fConst56 * fRec5[1]) + (fConst55 * (fRec6[0] + fRec6[1])));
		fRec4[0] = (fRec5[0] - (fTemp0 + fRec4[2]));
		double fTemp37 = (fRec4[2] + (fRec4[0] + fTemp0));
		fVec10[0] = fTemp37;
		fRec3[0] = ((1.0000000000000002 * (fVec10[0] + fVec10[1])) - (0.9999999999999998 * fRec3[1]));
		fRec52[0] = ((fConst69 * fRec52[1]) + (fConst68 * (fRec2[1] + fRec2[2])));
		fRec2[0] = (Ftube(TUBE_TABLE_12AX7_250k, ((fRec52[0] + fRec3[0]) - 0.840702)) - 147.47536585365856);
		fRec1[0] = ((fConst33 * fRec1[1]) + (fConst61 * ((fConst29 * fRec2[0]) + (fConst30 * fRec2[1]))));
		output0[i] = (FAUSTFLOAT)(fRec1[0] * fRec0[0]);
		// post processing
		fRec1[1] = fRec1[0];
		fRec2[2] = fRec2[1]; fRec2[1] = fRec2[0];
		fRec52[1] = fRec52[0];
		fRec3[1] = fRec3[0];
		fVec10[1] = fVec10[0];
		fRec4[2] = fRec4[1]; fRec4[1] = fRec4[0];
		fRec5[1] = fRec5[0];
		fRec6[1] = fRec6[0];
		fRec47[1] = fRec47[0];
		fRec48[2] = fRec48[1]; fRec48[1] = fRec48[0];
		fRec51[1] = fRec51[0];
		fRec49[1] = fRec49[0];
		fVec9[1] = fVec9[0];
		fRec50[2] = fRec50[1]; fRec50[1] = fRec50[0];
		fRec7[1] = fRec7[0];
		fRec8[2] = fRec8[1]; fRec8[1] = fRec8[0];
		fRec46[1] = fRec46[0];
		fRec9[1] = fRec9[0];
		fVec8[1] = fVec8[0];
		fRec10[2] = fRec10[1]; fRec10[1] = fRec10[0];
		fRec11[1] = fRec11[0];
		fVec7[1] = fVec7[0];
		fRec13[1] = fRec13[0];
		fRec41[1] = fRec41[0];
		fRec42[2] = fRec42[1]; fRec42[1] = fRec42[0];
		fRec45[1] = fRec45[0];
		fRec43[1] = fRec43[0];
		fVec6[1] = fVec6[0];
		fRec44[2] = fRec44[1]; fRec44[1] = fRec44[0];
		fRec14[1] = fRec14[0];
		fRec15[2] = fRec15[1]; fRec15[1] = fRec15[0];
		fRec40[1] = fRec40[0];
		fRec16[1] = fRec16[0];
		fVec5[1] = fVec5[0];
		fRec17[2] = fRec17[1]; fRec17[1] = fRec17[0];
		fRec18[1] = fRec18[0];
		fVec4[1] = fVec4[0];
		fRec38[2] = fRec38[1]; fRec38[1] = fRec38[0];
		fRec39[1] = fRec39[0];
		fRec34[2] = fRec34[1]; fRec34[1] = fRec34[0];
		fRec35[2] = fRec35[1]; fRec35[1] = fRec35[0];
		fRec36[2] = fRec36[1]; fRec36[1] = fRec36[0];
		fRec37[1] = fRec37[0];
		fRec31[2] = fRec31[1]; fRec31[1] = fRec31[0];
		fRec32[2] = fRec32[1]; fRec32[1] = fRec32[0];
		fRec33[1] = fRec33[0];
		fRec19[2] = fRec19[1]; fRec19[1] = fRec19[0];
		fRec20[1] = fRec20[0];
		fVec3[1] = fVec3[0];
		fRec21[2] = fRec21[1]; fRec21[1] = fRec21[0];
		fRec22[1] = fRec22[0];
		fVec2[1] = fVec2[0];
		fRec23[2] = fRec23[1]; fRec23[1] = fRec23[0];
		fRec24[1] = fRec24[0];
		fRec25[1] = fRec25[0];
		fVec1[1] = fVec1[0];
		fRec26[2] = fRec26[1]; fRec26[1] = fRec26[0];
		fRec30[1] = fRec30[0];
		fRec27[1] = fRec27[0];
		fRec28[1] = fRec28[0];
		IOTA = IOTA+1;
		fRec29[1] = fRec29[0];
		fRec12[1] = fRec12[0];
		fRec0[1] = fRec0[0];
	}
#undef fslider0
#undef fslider1
#undef fslider2
#undef fslider3
}

void __rt_func Dsp::compute_static(int count, FAUSTFLOAT *input0, FAUSTFLOAT *output0, PluginDef *p)
{
	static_cast<Dsp*>(p)->compute(count, input0, output0);
}

int Dsp::register_par(const ParamReg& reg)
{
	fslider3_ = reg.registerVar("gxdistortion.drive","","SA","",&fslider3, 0.35, 0.0, 1.0, 0.01);
	fslider2_ = reg.registerVar("gxdistortion.wet_dry","","SA","",&fslider2, 1e+02, 0.0, 1e+02, 1.0);
	fslider1_ = reg.registerVar("amp2.stage1.Pregain","","SA","",&fslider1, -6.0, -2e+01, 2e+01, 0.1);
	fslider0_ = reg.registerVar("amp2.stage2.gain1","","SA","",&fslider0, -6.0, -2e+01, 2e+01, 0.1);
	return 0;
}

int Dsp::register_params_static(const ParamReg& reg)
{
	return static_cast<Dsp*>(reg.plugin)->register_par(reg);
}

PluginDef *plugin() {
	return new Dsp();
}

void Dsp::del_instance(PluginDef *p)
{
	delete static_cast<Dsp*>(p);
}

} // end namespace gxamp9
