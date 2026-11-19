/* Copyright 2026 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include "nsmtracker.h"
#include "seqblock_stretchpoints_proc.h"

#define D(n)
//#define D(n) n

static inline int64_t get_seqblock_duration(const struct SeqBlock *seqblock)
{
  return seqblock->t.time2 - seqblock->t.time;
}

// Returns NULL if the seqblock has no stretch points.
// Note: The returned pointer may only be used immediately.
static const struct SeqBlockStretchPoint *RT_get_stretchpoints(const struct SeqBlock *seqblock, int *num_points)
{
  const struct SeqBlockStretchPoints *stretchpoints = &seqblock->stretchpoints;

  int buffer = stretchpoints->curr_buffer;

  int num = stretchpoints->num_stretchpoints[buffer];
  if (num < 2){
    *num_points = 0;
    return NULL;
  }

  *num_points = num;
  return stretchpoints->stretchpoints[buffer];
}

bool SEQBLOCK_STRETCHPOINTS_has(const struct SeqBlock *seqblock)
{
  int num_points;
  return RT_get_stretchpoints(seqblock, &num_points) != NULL;
}

int SEQBLOCK_STRETCHPOINTS_get_num(const struct SeqBlock *seqblock)
{
  int num_points;
  RT_get_stretchpoints(seqblock, &num_points);
  return num_points;
}

bool SEQBLOCK_STRETCHPOINTS_get(const struct SeqBlock *seqblock, int index, int64_t *seqtime, int64_t *sample_pos)
{
  int num_points;
  const struct SeqBlockStretchPoint *points = RT_get_stretchpoints(seqblock, &num_points);

  if (points == NULL)
    return false;

  if (index < 0 || index >= num_points)
    return false;

  *seqtime = points[index].seqtime;
  *sample_pos = points[index].sample_pos;

  return true;
}

static inline int64_t get_linear_sample_pos(const struct SeqBlock *seqblock, int64_t seqtime)
{
  const double stretchspeed = seqblock->t.stretch * seqblock->t.speed;

  if (stretchspeed <= 0.0)
    return seqblock->t.interior_start;

  return seqblock->t.interior_start + (int64_t)round((double)seqtime / stretchspeed);
}

int64_t SEQBLOCK_STRETCHPOINTS_get_sample_pos(const struct SeqBlock *seqblock, int64_t seqtime)
{
  int num_points;
  const struct SeqBlockStretchPoint *points = RT_get_stretchpoints(seqblock, &num_points);

  if (points == NULL)
    return get_linear_sample_pos(seqblock, seqtime);

  if (seqtime <= points[0].seqtime)
    return points[0].sample_pos;

  if (seqtime >= points[num_points-1].seqtime)
    return points[num_points-1].sample_pos;

  // Find the segment. (Binary search, since we might be called from the audio thread.)
  int low = 0;
  int high = num_points - 1;

  while(high - low > 1){
    int mid = low + (high-low)/2;
    if (seqtime < points[mid].seqtime)
      high = mid;
    else
      low = mid;
  }

  const struct SeqBlockStretchPoint *p1 = &points[low];
  const struct SeqBlockStretchPoint *p2 = &points[high];

#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE2(p2->seqtime > p1->seqtime, p1->sample_pos);
#endif

  const double fraction = (double)(seqtime - p1->seqtime) / (double)(p2->seqtime - p1->seqtime);

  return p1->sample_pos + (int64_t)round(fraction * (double)(p2->sample_pos - p1->sample_pos));
}

double SEQBLOCK_STRETCHPOINTS_get_stretch(const struct SeqBlock *seqblock, int64_t seqtime)
{
  int num_points;
  const struct SeqBlockStretchPoint *points = RT_get_stretchpoints(seqblock, &num_points);

  if (points == NULL)
    return seqblock->t.stretch;

  int low = 0;

  if (seqtime >= points[num_points-1].seqtime){
    low = num_points-2;
  } else if (seqtime > points[0].seqtime){
    int high = num_points - 1;

    while(high - low > 1){
      int mid = low + (high-low)/2;
      if (seqtime < points[mid].seqtime)
        high = mid;
      else
        low = mid;
    }
  }

  const struct SeqBlockStretchPoint *p1 = &points[low];
  const struct SeqBlockStretchPoint *p2 = &points[low+1];

  const int64_t sample_duration = p2->sample_pos - p1->sample_pos;

  if (sample_duration <= 0)
    return seqblock->t.stretch;

  // Same format as seqblock->t.stretch, i.e. speed is not included.
  double stretch = (double)(p2->seqtime - p1->seqtime) / (double)sample_duration;

  if (seqblock->t.speed > 0.0)
    stretch /= seqblock->t.speed;

  if (!(stretch > 0.0))
    return seqblock->t.stretch;

  return stretch;
}

double SEQBLOCK_STRETCHPOINTS_get_min_stretch(const struct SeqBlock *seqblock)
{
  int num_points;
  const struct SeqBlockStretchPoint *points = RT_get_stretchpoints(seqblock, &num_points);

  if (points == NULL)
    return seqblock->t.stretch;

  double min_stretch = -1.0;

  for(int i=0;i<num_points-1;i++){
    const int64_t sample_duration = points[i+1].sample_pos - points[i].sample_pos;

    if (sample_duration <= 0)
      continue;

    double stretch = (double)(points[i+1].seqtime - points[i].seqtime) / (double)sample_duration;

    if (seqblock->t.speed > 0.0)
      stretch /= seqblock->t.speed;

    if (stretch > 0.0 && (min_stretch < 0.0 || stretch < min_stretch))
      min_stretch = stretch;
  }

  if (min_stretch <= 0.0)
    return seqblock->t.stretch;

  return min_stretch;
}

static void set_points(struct SeqBlock *seqblock, const struct SeqBlockStretchPoint *points, int num_points, int64_t duration, int64_t interior_start, int64_t interior_end)
{
  struct SeqBlockStretchPoints *stretchpoints = &seqblock->stretchpoints;

  int write_buffer = 1 - stretchpoints->curr_buffer;

  struct SeqBlockStretchPoint *dest = stretchpoints->stretchpoints[write_buffer];

  if (num_points < 2 || duration <= 0 || interior_end <= interior_start){
    stretchpoints->num_stretchpoints[write_buffer] = 0;
    stretchpoints->curr_buffer = write_buffer;
    return;
  }

  if (num_points > MAX_SEQBLOCK_STRETCHPOINTS)
    num_points = MAX_SEQBLOCK_STRETCHPOINTS;

  int num = 0;

  for(int i=0;i<num_points;i++){

    int64_t seqtime = R_BOUNDARIES(0, points[i].seqtime, duration);
    int64_t sample_pos = R_BOUNDARIES(interior_start, points[i].sample_pos, interior_end);

    if (num > 0){
      // Drop points that are not strictly increasing.
      if (seqtime <= dest[num-1].seqtime || sample_pos <= dest[num-1].sample_pos)
        continue;
    }

    dest[num].seqtime = seqtime;
    dest[num].sample_pos = sample_pos;
    num++;
  }

  if (num < 2){
    stretchpoints->num_stretchpoints[write_buffer] = 0;
    stretchpoints->curr_buffer = write_buffer;
    return;
  }

  // The first and last stretch points are always the start and the end of the seqblock.
  dest[0].seqtime = 0;
  dest[0].sample_pos = interior_start;
  dest[num-1].seqtime = duration;
  dest[num-1].sample_pos = interior_end;

  // Only the start and end points is the same as no stretch points at all.
  if (num < 3){
    stretchpoints->num_stretchpoints[write_buffer] = 0;
    stretchpoints->curr_buffer = write_buffer;
    return;
  }

  stretchpoints->num_stretchpoints[write_buffer] = num;
  stretchpoints->curr_buffer = write_buffer;

  D(printf("STRETCHPOINTS set: %d points\n", num));

#if !defined(RELEASE)
  for(int i=1;i<num;i++){
    R_ASSERT_NON_RELEASE(dest[i].seqtime > dest[i-1].seqtime);
    R_ASSERT_NON_RELEASE(dest[i].sample_pos > dest[i-1].sample_pos);
  }
#endif
}

void SEQBLOCK_STRETCHPOINTS_set(struct SeqBlock *seqblock, const struct SeqBlockStretchPoint *points, int num_points)
{
  set_points(seqblock, points, num_points, get_seqblock_duration(seqblock), seqblock->t.interior_start, seqblock->t.interior_end);
}

void SEQBLOCK_STRETCHPOINTS_clear(struct SeqBlock *seqblock)
{
  SEQBLOCK_STRETCHPOINTS_set(seqblock, NULL, 0);
}

void SEQBLOCK_STRETCHPOINTS_duration_changed(struct SeqBlock *seqblock, int64_t old_duration, int64_t new_duration)
{
  int num_points;
  const struct SeqBlockStretchPoint *points = RT_get_stretchpoints(seqblock, &num_points);

  if (points == NULL)
    return;

  if (old_duration <= 0 || new_duration <= 0 || old_duration == new_duration)
    return;

  struct SeqBlockStretchPoint rescaled[MAX_SEQBLOCK_STRETCHPOINTS];

  const double factor = (double)new_duration / (double)old_duration;

  for(int i=0;i<num_points;i++){
    rescaled[i].seqtime = (int64_t)round((double)points[i].seqtime * factor);
    rescaled[i].sample_pos = points[i].sample_pos;
  }

  set_points(seqblock, rescaled, num_points, new_duration, seqblock->t.interior_start, seqblock->t.interior_end);
}

