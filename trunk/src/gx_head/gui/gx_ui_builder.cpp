/*
 * Copyright (C) 2009, 2010 Hermann Meyer, James Warden, Andreas Degert
 * Copyright (C) 2011 Pete Shorthose
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * ---------------------------------------------------------------------------
 *
 *    This is the gx_head GUI main class
 *
 * ----------------------------------------------------------------------------
 */

#include "guitarix.h"         // NOLINT

#include <iomanip>            // NOLINT
#include <cstring>            // NOLINT
#include <string>             // NOLINT
#include <gxw/GxControlParameter.h>

namespace gx_gui {

/* ----- load a top level window from gtk builder file ------ */

GtkWidget *load_toplevel(GtkBuilder *builder, const char* filename, const char* windowname) {
    string fname = gx_system::get_options().get_builder_filepath(filename);
    GError *err = NULL;
    if (!gtk_builder_add_from_file(builder, fname.c_str(), &err)) {
        g_object_unref(G_OBJECT(builder));
        gx_system::gx_print_fatal(_("gtk builder"), err->message);
        g_error_free(err);
        return NULL;
    }
    GtkWidget *w = GTK_WIDGET(gtk_builder_get_object(builder, windowname));
    if (!w) {
        g_object_unref(G_OBJECT(builder));
        gx_system::gx_print_fatal(_("gtk builder"), string(windowname)+_(" not found in ")+fname);
        return NULL;
    }
    gtk_builder_connect_signals(builder, 0);
    return w;
}

/****************************************************************
 ** UiBuilder implementation
 */

StackBoxBuilder *UiBuilderImpl::intf = 0;

UiBuilderImpl::UiBuilderImpl(MainWindow *i, gx_gui::StackBoxBuilder *b, std::vector<PluginUI*> *pl)
    : UiBuilderBase(), main(*i), pluginlist(pl) {
    intf = b;
    openVerticalBox = openVerticalBox_;
    openVerticalBox1 = openVerticalBox1_;
    openVerticalBox2 = openVerticalBox2_;
    openHorizontalBox = openHorizontalBox_;
    openHorizontalhideBox = openHorizontalhideBox_;
    closeBox = closeBox_;
    load_glade = load_glade_;
    create_master_slider = create_master_slider_;
    create_small_rackknob = create_small_rackknob_;
    create_small_rackknobr = create_small_rackknobr_;
    create_spin_value = create_spin_value_;
    create_switch = create_switch_;
    create_switch_no_caption = create_switch_no_caption_;
    create_selector = create_selector_;
    create_selector_no_caption = create_selector_no_caption_;
    create_port_display = create_port_display_;
    insertSpacer = insertSpacer_;
};

bool UiBuilderImpl::load_unit(PluginUI &pl) {
    PluginDef *pd = pl.plugin->pdef;
    if (!pd->load_ui) {
	return false;
    }
    intf->prepare();
    plugin = pd;
    pd->load_ui(*this);
    return true;
}

void UiBuilderImpl::openVerticalBox_(const char* label) {
    intf->openVerticalBox(label);
}

void UiBuilderImpl::openVerticalBox1_(const char* label) {
    intf->openVerticalBox1(label);
}

void UiBuilderImpl::openVerticalBox2_(const char* label) {
    intf->openVerticalBox2(label);
}

void UiBuilderImpl::openHorizontalhideBox_(const char* label) {
    intf->openHorizontalhideBox(label);
}

void UiBuilderImpl::openHorizontalBox_(const char* label) {
    intf->openHorizontalBox(label);
}

void UiBuilderImpl::insertSpacer_() {
    intf->openSpaceBox("");
    intf->closeBox();
}

void UiBuilderImpl::create_small_rackknob_(const char *id, const char *label) {
    if (label) {
	intf->create_small_rackknob(id, label);
    } else {
	intf->create_small_rackknob(id);
    }
}

void UiBuilderImpl::create_small_rackknobr_(const char *id, const char *label) {
    if (label) {
	intf->create_small_rackknobr(id, label);
    } else {
	intf->create_small_rackknobr(id);
    }
}

void UiBuilderImpl::create_master_slider_(const char *id, const char *label) {
    if (label) {
	intf->create_master_slider(id, label);
    } else {
	intf->create_master_slider(id);
    }
}

void UiBuilderImpl::create_selector_no_caption_(const char *id) {
    intf->create_selector(id, "");
}

void UiBuilderImpl::create_selector_(const char *id, const char *label) {
    intf->create_selector_with_caption(id, label);
}

void UiBuilderImpl::create_spin_value_(const char *id, const char *label) {
    intf->create_spin_value(id, label);
}

void UiBuilderImpl::create_switch_no_caption_(const char *sw_type, const char * id) {
    intf->create_switch_no_caption(sw_type, id);
}

void UiBuilderImpl::create_switch_(const char *sw_type, const char * id, const char *label) {
    intf->create_switch(sw_type, id, label, Gtk::POS_TOP);
}

void UiBuilderImpl::create_port_display_(const char *id, const char *label) {
    intf->create_port_display(id, label);
}

void UiBuilderImpl::closeBox_() {
    intf->closeBox();
}

void UiBuilderImpl::load_glade_(const char *data) {
    intf->loadRackFromGladeData(data);
}

bool UiBuilderImpl::load(gx_engine::Plugin *p) {
    PluginDef *pd = p->pdef;
    if (!(pd->flags & PGN_GUI) || !(pd->flags & gx_engine::PGNI_DYN_POSITION)) {
	return false;
    }
    main.add_plugin(*pluginlist, pd->id, "", "");
    return true;
}

} /* end of gx_gui namespace */

/****************************************************************
 ** class GxBuilder
 */

// GList(GObject*) helper class must be defined in other namespace
namespace Glib { namespace Container_Helpers {
template <>
struct TypeTraits<GObject*> {
    typedef GObject *CppType;
    typedef GObject *CType;
    typedef GObject *CTypeNonConst;

    static CType to_c_type(CppType item) { return item; }
    static CppType to_cpp_type(CType item) { return item; }
    static void release_c_type(CType) {}
};
}}

namespace gx_gui {

//static
Glib::RefPtr<GxBuilder> GxBuilder::create_from_file(
    const std::string& filename, gx_ui::GxUI* ui, const char* object_id) {
    Glib::RefPtr<GxBuilder> builder = GxBuilder::create();
    try {
	if (object_id) {
	    builder->add_from_file(filename, object_id);
	} else {
	    builder->add_from_file(filename);
	}
    } catch(const Glib::FileError& ex) {
        gx_system::gx_print_fatal("FileError", ex.what());
    } catch(const Gtk::BuilderError& ex) {
        gx_system::gx_print_fatal("Builder Error", ex.what());
    }
    if (ui) {
	builder->fixup_controlparameters(*ui);
    }
    return builder;
}

//static
Glib::RefPtr<GxBuilder> GxBuilder::create_from_file(
    const std::string& filename, gx_ui::GxUI* ui, const Glib::StringArrayHandle& object_ids) {
    Glib::RefPtr<GxBuilder> builder = GxBuilder::create();
    try {
	builder->add_from_file(filename, object_ids);
    } catch(const Glib::FileError& ex) {
        gx_system::gx_print_fatal("FileError", ex.what());
    } catch(const Gtk::BuilderError& ex) {
        gx_system::gx_print_fatal("Builder Error", ex.what());
    }
    if (ui) {
	builder->fixup_controlparameters(*ui);
    }
    return builder;
}

//static
Glib::RefPtr<GxBuilder> GxBuilder::create_from_string(
    const Glib::ustring& buffer, gx_ui::GxUI* ui, const char* object_id) {
    Glib::RefPtr<GxBuilder> builder = GxBuilder::create();
    try {
	if (object_id) {
	    builder->add_from_string(buffer, object_id);
	} else {
	    builder->add_from_string(buffer);
	}
    } catch(const Gtk::BuilderError& ex) {
        gx_system::gx_print_fatal("Builder Error", ex.what());
    }
    if (ui) {
	builder->fixup_controlparameters(*ui);
    }
    return builder;
}

//static
Glib::RefPtr<GxBuilder> GxBuilder::create_from_string(
    const Glib::ustring& buffer, gx_ui::GxUI* ui, const Glib::StringArrayHandle& object_ids) {
    Glib::RefPtr<GxBuilder> builder = GxBuilder::create();
    try {
	builder->add_from_string(buffer, object_ids);
    } catch(const Gtk::BuilderError& ex) {
        gx_system::gx_print_fatal("Builder Error", ex.what());
    }
    if (ui) {
	builder->fixup_controlparameters(*ui);
    }
    return builder;
}

Gtk::Window *GxBuilder::get_first_window() {
    Glib::SListHandle<GObject*> objs = Glib::SListHandle<GObject*>(
	gtk_builder_get_objects(gobj()), Glib::OWNERSHIP_DEEP);
    for (Glib::SListHandle<GObject*>::iterator i = objs.begin(); i != objs.end(); ++i) {
	if (g_type_is_a(G_OBJECT_TYPE(*i), GTK_TYPE_WINDOW)) {
	    return Glib::wrap(GTK_WINDOW(*i), false);
	}
    }
    assert(false);
    return 0;
}

GObject* GxBuilder::get_cobject(const Glib::ustring& name)
{
    GObject *cobject = gtk_builder_get_object (gobj(), name.c_str());
    if(!cobject) {
	g_critical("gtkmm: object `%s' not found in GtkBuilder file.", name.c_str());
	return 0;
    }

#if 0
    if (!GTK_IS_WIDGET(cobject))  {
	g_critical("gtkmm: object `%s' (type=`%s') (in GtkBuilder file) is not a widget type.",
		   name.c_str(), G_OBJECT_TYPE_NAME(cobject));
	/* TODO: Unref/sink it? */
	return 0;
    }
#endif

  return cobject;
}

Gtk::Object* GxBuilder::get_widget_checked(const Glib::ustring& name, GType type, bool take_ref) {
    GObject *cobject = get_cobject(name);
    if(!cobject) {
	g_critical("gtkmm: GxBuilder: widget `%s' was not found in the GtkBuilder file, or the specified part of it.", 
		   name.c_str());
	return 0;
    }
    if(!g_type_is_a(G_OBJECT_TYPE(cobject), type)) {
	g_critical("gtkmm: widget `%s' (in GtkBuilder file) is of type `%s' but `%s' was expected",
		   name.c_str(), G_OBJECT_TYPE_NAME(cobject), g_type_name(type));
	return 0;
    }
    return Glib::wrap (GTK_OBJECT(cobject), take_ref);
}

/*
 ** GxBuilder::fixup_controlparameters + helper classes
 */

template<class T>
class uiSelector: public gx_ui::GxUiItemV<T> {
protected:
    Gtk::Range *rng;
    void on_value_changed();
    virtual void reflectZone();
public:
    uiSelector(gx_ui::GxUI& ui, Gtk::Range *rng, T* zone);
};

template<class T>
uiSelector<T>::uiSelector(gx_ui::GxUI& ui, Gtk::Range *rng_, T* zone)
    : gx_ui::GxUiItemV<T>(&ui, zone), rng(rng_) {
    rng->set_value(*zone);
    rng->signal_value_changed().connect(
	sigc::mem_fun(*this, &uiSelector<T>::on_value_changed));
}

template<class T>
void uiSelector<T>::on_value_changed() {
    this->modifyZone(static_cast<T>(rng->get_value()));
}

template<class T>
void uiSelector<T>::reflectZone() {
    T v = *gx_ui::GxUiItemV<T>::fZone;
    gx_ui::GxUiItemV<T>::fCache = v;
    rng->set_value(v);
}

static void widget_destroyed(gpointer data) {
    delete static_cast<gx_ui::GxUiItem*>(data);
}

static void destroy_with_widget(Glib::Object *t, gx_ui::GxUiItem *p) {
    t->set_data("GxUiItem", p, widget_destroyed);
}

static void make_switch_controller(gx_ui::GxUI& ui, Glib::RefPtr<Gxw::ControlParameter>& w, gx_engine::Parameter& p) {
    w->cp_configure(p.l_group(), p.l_name(), 0, 0, 0);
    Gtk::ToggleButton *t = dynamic_cast<Gtk::ToggleButton*>(w.operator->());
    if (p.isFloat()) {
	gx_engine::FloatParameter &fp = p.getFloat();
	w->cp_set_value(fp.get_value());
	if (t) {
	    destroy_with_widget(t, new uiToggle<float>(ui, t, &fp.get_value()));
	}
    } else if (p.isBool()) {
	gx_engine::BoolParameter &fp = p.getBool();
	w->cp_set_value(fp.get_value());
	if (t) {
	    destroy_with_widget(t, new uiToggle<bool>(ui, t, &fp.get_value()));
	}
    } else {
	gx_system::gx_print_warning(
	    "load dialog",
	    Glib::ustring::compose("Switch Parameter variable %1: type not handled", p.id()));
    }
}

struct uiAdjustmentLog : public gx_ui::GxUiItemFloat {
    GtkAdjustment* fAdj;
    uiAdjustmentLog(gx_ui::GxUI* ui, float* zone, GtkAdjustment* adj) :
	gx_ui::GxUiItemFloat(ui, zone), fAdj(adj) {
	gtk_adjustment_set_value(fAdj, log10(*zone));
    }
    static void changed(GtkAdjustment *adj, gpointer data) {
	float    v = adj->value;
	((gx_ui::GxUiItemFloat*)data)->modifyZone(pow(10.0,v));
    }
    virtual void reflectZone() {
	float     v = *fZone;
	fCache = v;
	gtk_adjustment_set_value(fAdj, log10(v));
    }
};

static void make_continuous_controller(gx_ui::GxUI& ui, Glib::RefPtr<Gxw::ControlParameter>& w, gx_engine::Parameter& p) {
    Glib::RefPtr<Gxw::Regler> r = Glib::RefPtr<Gxw::Regler>::cast_dynamic(w);
    if (!r) {
	make_switch_controller(ui, w, p);
	return;
    }
    if (!p.isFloat()) {
	gx_system::gx_print_warning(
	    "load dialog",
	    Glib::ustring::compose("Continuous Parameter variable %1: type not handled", p.id()));
	return;
    }
    Gtk::Adjustment *adj = r->get_adjustment();
    gx_engine::FloatParameter &fp = p.getFloat();
    if (fp.is_log_display()) {
	double up = log10(fp.upper);
	double step = log10(fp.step);
	w->cp_configure(fp.l_group(), fp.l_name(), log10(fp.lower), up, step);
	int prec = 0;
	float d = log10((fp.step-1)*fp.upper);
	if (up > 0) {
	    prec = up;
	    if (d < 0) {
		prec -= floor(d);
	    }
	} else if (d < 0) {
	    prec = -floor(d);
	}
	r->signal_format_value().connect(
	    sigc::bind(
		sigc::ptr_fun(logarithmic_format_value),
		prec));
	r->signal_input_value().connect(
	    sigc::ptr_fun(logarithmic_input_value));
	w->cp_set_value(log10(fp.get_value()));
	gx_gui::uiAdjustmentLog* c = new gx_gui::uiAdjustmentLog(&ui, &fp.get_value(), adj->gobj());
	adj->signal_value_changed().connect(
	    sigc::bind<GtkAdjustment*>(
		sigc::bind<gpointer>(
		    sigc::ptr_fun(gx_gui::uiAdjustmentLog::changed),
		    (gpointer)c), adj->gobj()));
	destroy_with_widget(r.operator->(), c);
    } else {
	w->cp_configure(p.l_group(), p.l_name(), fp.lower, fp.upper, fp.step);
	w->cp_set_value(fp.get_value());
	gx_gui::uiAdjustment* c = new gx_gui::uiAdjustment(&ui, &fp.get_value(), adj->gobj());
	adj->signal_value_changed().connect(
	    sigc::bind<GtkAdjustment*>(
		sigc::bind<gpointer>(
		    sigc::ptr_fun(gx_gui::uiAdjustment::changed),
		    (gpointer)c), adj->gobj()));
	destroy_with_widget(r.operator->(), c);
    }
}

static void make_enum_controller(gx_ui::GxUI& ui, Glib::RefPtr<Gxw::ControlParameter>& w, gx_engine::Parameter& p) {
    Gxw::Selector *t = dynamic_cast<Gxw::Selector*>(w.operator->());
    if (!t) {
	make_continuous_controller(ui, w, p);
	return;
    }
    Gtk::TreeModelColumn<Glib::ustring> label;
    Gtk::TreeModelColumnRecord rec;
    rec.add(label);
    Glib::RefPtr<Gtk::ListStore> ls = Gtk::ListStore::create(rec);
    for (const value_pair *vp = p.getValueNames(); vp->value_id; ++vp) {
	ls->append()->set_value(0, Glib::ustring(p.value_label(*vp)));
    }
    t->set_model(ls);
    w->cp_configure(p.l_group(), p.l_name(), p.getLowerAsFloat(), p.getUpperAsFloat(), 1.0);
    if (p.isInt()) {
	int& val = p.getInt().get_value();
	destroy_with_widget(t, new uiSelector<int>(ui, t, &val));
	t->cp_set_value(val);
    } else if (p.isUInt()) {
	unsigned int& val = p.getUInt().get_value();
	destroy_with_widget(t, new uiSelector<unsigned int>(ui, t, &val));
	t->cp_set_value(val);
    } else if (p.isFloat()) {
	float& val = p.getFloat().get_value();
	destroy_with_widget(t, new uiSelector<float>(ui, t, &val));
	t->cp_set_value(val);
    } else {
	gx_system::gx_print_warning(
	    "load dialog",
	    Glib::ustring::compose("Enum Parameter variable %1: type not handled", p.id()));
    }
}

void GxBuilder::fixup_controlparameters(gx_ui::GxUI& ui) {
    Glib::SListHandle<GObject*> objs = Glib::SListHandle<GObject*>(
        gtk_builder_get_objects(gobj()), Glib::OWNERSHIP_DEEP);
    for (Glib::SListHandle<GObject*>::iterator i = objs.begin(); i != objs.end(); ++i) {
	const char *wname = 0;
	if (g_type_is_a(G_OBJECT_TYPE(*i), GTK_TYPE_WIDGET)) {
	    const char *id = gtk_buildable_get_name(GTK_BUILDABLE(*i));
	    wname = g_strstr_len(id, -1, ":");
	    if (wname) {
		gtk_widget_set_name(GTK_WIDGET(*i), wname+1);
	    }
	}
        if (!g_type_is_a(G_OBJECT_TYPE(*i), GX_TYPE_CONTROL_PARAMETER)) {
            continue;
        }
        Glib::RefPtr<Gxw::ControlParameter> w = Glib::wrap(GX_CONTROL_PARAMETER(*i), true);
        Glib::ustring v = w->cp_get_var();
        if (v.empty()) {
            continue;
        }
	if (!wname) {
	    Glib::RefPtr<Gtk::Widget>::cast_dynamic(w)->set_name(v);
	}
        if (!gx_engine::parameter_map.hasId(v)) {
	    Glib::RefPtr<Gtk::Widget> wd = Glib::RefPtr<Gtk::Widget>::cast_dynamic(w);
	    wd->set_sensitive(0);
            wd->set_tooltip_text(v);
            gx_system::gx_print_warning(
		"load dialog",
		(boost::format("Parameter variable %1% not found") % v).str());
            continue;
        }
        gx_engine::Parameter& p = gx_engine::parameter_map[v];
        if (!p.desc().empty()) {
            Glib::RefPtr<Gtk::Widget>::cast_dynamic(w)->set_tooltip_text(
		gettext(p.desc().c_str()));
        }
	switch (p.getControlType()) {
	case gx_engine::Parameter::None:       assert(false); break;
	case gx_engine::Parameter::Continuous: make_continuous_controller(ui, w, p); break;
	case gx_engine::Parameter::Switch:     make_switch_controller(ui, w, p); break;
	case gx_engine::Parameter::Enum:       make_enum_controller(ui, w, p); break;
	default:         assert(false); break;
        }
	if (p.isControllable()) {
	    gx_gui::connect_midi_controller(GTK_WIDGET(w->gobj()), p.zone());
	}
    }
}

} /* end of gx_gui namespace */


