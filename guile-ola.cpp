/*
 * guile-ola.cpp
 *
 * A very thin Guile wrapper for OLA StreamingClient
 *
 */

#include <iostream>
#include <ola/DmxBuffer.h>
#include <ola/Logging.h>
#include <ola/client/StreamingClient.h>
#include <libguile.h>

using ola::client::StreamingClient;


static SCM dmxbuffer_type;
static SCM streamingclient_type;


static SCM make_ola_dmx_buffer()
{
	ola::DmxBuffer *buf = new ola::DmxBuffer;
	buf->Blackout();
	return scm_make_foreign_object_1(dmxbuffer_type, buf);
}


static void finalize_dmxbuffer(SCM obj)
{
	void *vp;
	scm_assert_foreign_object_type(dmxbuffer_type, obj);
	vp = scm_foreign_object_ref(obj, 0);
	ola::DmxBuffer *buf = static_cast<ola::DmxBuffer*>(vp);
	delete buf;
}


static SCM set_ola_dmx_buffer(SCM obj1, SCM obj2, SCM obj3)
{
	void *vp;
	scm_assert_foreign_object_type(dmxbuffer_type, obj1);
	vp = scm_foreign_object_ref(obj1, 0);
	ola::DmxBuffer *buf = static_cast<ola::DmxBuffer*>(vp);
	buf->SetChannel(scm_to_int(obj2), scm_to_int(obj3));
	return obj3;
}


static SCM ola_dmx_buffers_equal_p(SCM obj1, SCM obj2)
{
	void *vp1;
	void *vp2;
	scm_assert_foreign_object_type(dmxbuffer_type, obj1);
	scm_assert_foreign_object_type(dmxbuffer_type, obj2);
	vp1 = scm_foreign_object_ref(obj1, 0);
	vp2 = scm_foreign_object_ref(obj2, 0);
	ola::DmxBuffer *buf1 = static_cast<ola::DmxBuffer*>(vp1);
	ola::DmxBuffer *buf2 = static_cast<ola::DmxBuffer*>(vp2);
	return scm_from_bool(*buf1 == *buf2);
}


static SCM make_ola_streaming_client()
{
	StreamingClient *cl;
	StreamingClient::Options *opts;

	ola::InitLogging(ola::OLA_LOG_WARN, ola::OLA_LOG_STDERR);
	opts = new StreamingClient::Options;
	cl = new StreamingClient(opts);

	if ( !cl->Setup() ) {
		std::cerr << "Setup failed" << std::endl;
		return SCM_BOOL_F;
	} else {
		std::cerr << "Setup OK" << std::endl;
		return scm_make_foreign_object_1(streamingclient_type, cl);
	}
}


static void finalize_streamingclient(SCM obj)
{
	void *vp;
	scm_assert_foreign_object_type(streamingclient_type, obj);
	vp = scm_foreign_object_ref(obj, 0);
	StreamingClient *cl = static_cast<StreamingClient*>(vp);
	delete cl;
}


static SCM send_streaming_dmx_data(SCM obj1, SCM obj2, SCM obj3)
{
	void *vp1;
	void *vp3;
	scm_assert_foreign_object_type(streamingclient_type, obj1);
	scm_assert_foreign_object_type(dmxbuffer_type, obj3);
	vp1 = scm_foreign_object_ref(obj1, 0);
	vp3 = scm_foreign_object_ref(obj3, 0);
	StreamingClient *cl = static_cast<StreamingClient*>(vp1);
	ola::DmxBuffer *buf = static_cast<ola::DmxBuffer*>(vp3);
	if ( !cl->SendDmx(scm_to_int(obj2), *buf) ) {
		std::cerr << "DMX send failed" << std::endl;
	}
	return SCM_UNSPECIFIED;
}


extern "C" void init_guile_ola()
{
	SCM name, slots;

	name = scm_from_utf8_symbol("OlaDmxBuffer");
	slots = scm_list_1(scm_from_utf8_symbol("data"));
	dmxbuffer_type = scm_make_foreign_object_type(name,
	                                              slots,
	                                              finalize_dmxbuffer);
	scm_c_define_gsubr("make-ola-dmx-buffer",
	                   0, 0, 0,
	                   reinterpret_cast<scm_t_subr>(make_ola_dmx_buffer));
	scm_c_define_gsubr("set-ola-dmx-buffer!",
	                   3, 0, 0,
	                   reinterpret_cast<scm_t_subr>(set_ola_dmx_buffer));
	scm_c_define_gsubr("ola-dmx-buffers-equal?",
	                   2, 0, 0,
	                   reinterpret_cast<scm_t_subr>(ola_dmx_buffers_equal_p));

	name = scm_from_utf8_symbol("OlaStreamingClient");
	slots = scm_list_1(scm_from_utf8_symbol("data"));
	streamingclient_type = scm_make_foreign_object_type(name,
	                                                    slots,
	                                                    finalize_streamingclient);
	scm_c_define_gsubr("make-ola-streaming-client",
	                   0, 0, 0,
	                   reinterpret_cast<scm_t_subr>(make_ola_streaming_client));
	scm_c_define_gsubr("send-streaming-dmx-data!",
	                   3, 0, 0,
	                   reinterpret_cast<scm_t_subr>(send_streaming_dmx_data));

	scm_add_feature("guile-ola");
}
