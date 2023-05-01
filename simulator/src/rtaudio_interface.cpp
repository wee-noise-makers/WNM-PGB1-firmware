#include "rtaudio/RtAudio.h"
#include <map>

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

#if _WIN32
const RtAudio::Api api = RtAudio::WINDOWS_WASAPI;
#elif __MACH__
const RtAudio::Api api = RtAudio::MACOSX_CORE;
#else
const RtAudio::Api api = RtAudio::LINUX_PULSE;
#endif

static const std::map<RtAudio::Api, std::string> API_NAMES = {
  { RtAudio::LINUX_ALSA, "ALSA" },
  { RtAudio::UNIX_JACK, "JACK" },
  { RtAudio::LINUX_PULSE, "PulseAudio" },
  { RtAudio::LINUX_OSS, "OSS" },
  { RtAudio::WINDOWS_WASAPI, "WASAPI" },
  { RtAudio::WINDOWS_ASIO, "ASIO" },
  { RtAudio::WINDOWS_DS, "DirectSound" },
  { RtAudio::MACOSX_CORE, "Core Audio" },
  { RtAudio::RTAUDIO_DUMMY, "Dummy" },
  { RtAudio::UNSPECIFIED, "Unspecified" },
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
  RtAudio *audio = NULL;

  try
    {
      std::cout << "Trying to open API:" << API_NAMES.at (api);
      audio = new RtAudio (api);
    }
  catch (RtAudioError &error)
    {
      // Handle the exception here
      error.printMessage ();
      return 1;
    }

  if (audio == NULL)
    {
      return 1;
    }
  else
    {
      std::cout << "Success open API:" << API_NAMES.at (api);
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
  try
    {
      audio->openStream (outParam, NULL, RTAUDIO_SINT16, sample_rate, &bufsize,
                         rtaudio_callback, NULL);

      audio->startStream ();
    }
  catch (RtAudioError &error)
    {
      // Handle the exception here
      error.printMessage ();
      return 1;
    }

  return 0;
}

int
wnm_rtaudio_cleanup (void)
{
  // Clean up
  delete audio;

  return 0;
}
