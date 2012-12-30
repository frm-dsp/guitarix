/*
 * Copyright (C) 2012 Hermann Meyer, Andreas Degert, Pete Shorthose
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
 * --------------------------------------------------------------------------
 */

Impf::Impf()
{
}

Impf::~Impf()
{
}

inline void Impf::init(uint32_t samplingFreq)
{
  fSamplingFreq = samplingFreq;
}

inline void Impf::compute(int32_t count, float *input0, float *output0, float value)
{
  double 	fSlow0 = value* value * 0.01;
  //double 	fSlow1 = (fSlow0 * pow(10,(0 - (0.1 * fSlow0))));
  for (int32_t i=0; i<count; i++)
    {
      output0[i] = fSlow0 * input0[i];
    }
}

void Impf::connect(uint32_t port,void* data)
{
  switch ((PortIndex)port)
    {
    case AMP_MASTERGAIN:
      break;
    case AMP_PREGAIN:
      break;
    case AMP_WET_DRY:
      break;
    case AMP_DRIVE:
      break;
    case MID:
      break;
    case BASS:
      break;
    case TREBLE:
      break;
    case CLevel:
      break;
    case ALevel:
      break;
    case AMP_CONTROL:
      break;
    case AMP_NOTIFY:
      break;
    case AMP_OUTPUT:
      break;
    case AMP_INPUT:
      break;
    case AMP_OUTPUT1:
      break;
    case AMP_INPUT1:
      break;
    }
}

void Impf::connect_static(uint32_t port,void* data, Impf *p)
{
  static_cast<Impf*>(p)->connect(port, data);
}

void Impf::init_static(uint32_t samplingFreq, Impf *p)
{
  static_cast<Impf*>(p)->init(samplingFreq);
}

