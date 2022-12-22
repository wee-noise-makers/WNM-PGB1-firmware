#include "rtaudio/RtAudio.h"

RtAudio *audio = NULL;

#ifdef __cplusplus
extern "C" {
#endif

int wnm_rtaudio_init (unsigned int sample_rate, unsigned int frames);

int wnm_rtaudio_cleanup (void);

extern void wnm_rtaudio_callback (void *outbuf, unsigned int nFrames);

#ifdef __cplusplus
}
#endif

RtAudio::Api APIs_By_Prefered_Order[] =
{
 RtAudio::LINUX_PULSE, RtAudio::LINUX_ALSA, RtAudio::UNIX_JACK,
 RtAudio::LINUX_OSS,  RtAudio::WINDOWS_WASAPI, RtAudio::WINDOWS_ASIO,
 RtAudio::WINDOWS_DS, RtAudio::MACOSX_CORE, RtAudio::UNSPECIFIED
};

static int
rtaudio_callback(
	void			*outbuf,
	void			*inbuf,
	unsigned int		nFrames,
	double			streamtime,
	RtAudioStreamStatus	status,
	void			*userdata)
{
  (void)inbuf;
  unsigned int remainFrames;

  wnm_rtaudio_callback(outbuf, nFrames);

  return 0;
}

int
wnm_rtaudio_init (unsigned int sample_rate, unsigned int frames)
{
  RtAudio *audio = 0;

  for (int i = 0; APIs_By_Prefered_Order[i] != RtAudio::UNSPECIFIED; i++)
    {
      if (APIs_By_Prefered_Order[i] == RtAudio::UNSPECIFIED)
        {
          return 1;
        }
      try
      {
        // std::cout << "Trying to open API:" <<  API_Image.at(api));
        audio = new RtAudio (APIs_By_Prefered_Order[i]);
        break;
      }
      catch (RtAudioError &error)
      {
        // Handle the exception here
        error.printMessage ();
      }
    }

  /* probe audio devices */
  unsigned int devId = audio->getDefaultOutputDevice ();
  unsigned int bufsize = frames;

  /* Setup output stream parameters */
  RtAudio::StreamParameters *outParam = new RtAudio::StreamParameters ();
  outParam->deviceId = devId;
  outParam->nChannels = 2;
  outParam->firstChannel = 0;

  RtAudio::StreamOptions *options = new RtAudio::StreamOptions ();

  options->flags |= RTAUDIO_SCHEDULE_REALTIME;
  audio->openStream (outParam, NULL, RTAUDIO_SINT16, sample_rate, &bufsize,
                     rtaudio_callback, NULL);

  audio->startStream ();

  return 0;
}

int
wnm_rtaudio_cleanup (void)
{
  // Clean up
  delete audio;

  return 0;
}
