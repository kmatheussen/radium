#include <stdio.h>
#include <math.h>

#include <string.h>

#include "../common/nsmtracker.h"
#include "../common/seqblock_stretchpoints_proc.h"

static struct SeqBlock *create_test_seqblock(int64_t duration, int64_t interior_start, int64_t interior_end, double speed){
  static struct SeqBlock seqblock;
  memset(&seqblock, 0, sizeof(seqblock));
  seqblock.t.time = 480000;
  seqblock.t.time2 = 480000 + duration;
  seqblock.t.interior_start = interior_start;
  seqblock.t.interior_end = interior_end;
  seqblock.t.num_samples = interior_end - interior_start;
  seqblock.t.default_duration = interior_end - interior_start;
  seqblock.t.speed = speed;
  seqblock.t.stretch = ((double)duration / (double)(interior_end - interior_start)) / speed;
  return &seqblock;
}

#define CHECK(what, got, expected) do { \
  if ((got) != (expected)) { printf("   FAILED: %s. Got %d, expected %d\n", what, (int)(got), (int)(expected)); num_failures++; } \
  else printf("   ok: %s = %d\n", what, (int)(got)); \
} while(0)

int main(void){
  int num_failures = 0;

  // 1000 frames of audio (interior 0..1000), no stretch.
  {
    printf("1. No stretch points:\n");
    struct SeqBlock *seqblock = create_test_seqblock(1000, 0, 1000, 1.0);

    CHECK("has", SEQBLOCK_STRETCHPOINTS_has(seqblock), false);
    CHECK("num", SEQBLOCK_STRETCHPOINTS_get_num(seqblock), 0);
    CHECK("sample pos at 0", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 0), 0);
    CHECK("sample pos at 500", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 500), 500);
    CHECK("sample pos at 1000", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 1000), 1000);
  }

  // One stretch point at 25% of the timeline. Audio content: the block is
  // stretched to twice its natural length, so 1000 frames of audio is played in
  // 2000 frames, and the point at 500 maps to sample 250.
  {
    printf("2. One stretch point:\n");
    struct SeqBlock *seqblock = create_test_seqblock(2000, 0, 1000, 1.0);

    struct SeqBlockStretchPoint points[3] = {
      {0, 0},
      {400, 250},
      {2000, 1000}
    };

    SEQBLOCK_STRETCHPOINTS_set(seqblock, points, 3);

    CHECK("has", SEQBLOCK_STRETCHPOINTS_has(seqblock), true);
    CHECK("num", SEQBLOCK_STRETCHPOINTS_get_num(seqblock), 3);
    CHECK("sample pos at 0", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 0), 0);
    CHECK("sample pos at 200", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 200), 125);
    CHECK("sample pos at 400", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 400), 250);
    CHECK("sample pos at 1200", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 1200), 625);
    CHECK("sample pos at 2000", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 2000), 1000);

    // Segment 0: 400 timeline frames / 250 sample frames. stretch = 400/250 = 1.6, / speed(1) = 1.6
    if (fabs(SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 200) - 1.6) > 0.0001){
      printf("   FAILED: stretch segment 0: %f\n", SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 200));
      num_failures++;
    } else
      printf("   ok: stretch segment 0 = 1.6\n");

    // Segment 1: (2000-400)/(1000-250) = 1600/750 = 2.1333
    if (fabs(SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 1200) - 1600.0/750.0) > 0.0001){
      printf("   FAILED: stretch segment 1: %f\n", SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 1200));
      num_failures++;
    } else
      printf("   ok: stretch segment 1 = %f\n", SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 1200));

    if (fabs(SEQBLOCK_STRETCHPOINTS_get_min_stretch(seqblock) - 1.6) > 0.0001){
      printf("   FAILED: min stretch: %f\n", SEQBLOCK_STRETCHPOINTS_get_min_stretch(seqblock));
      num_failures++;
    } else
      printf("   ok: min stretch = 1.6\n");

    // Only the start and end points should have been dropped.
    struct SeqBlockStretchPoint only_ends[2] = {{0,0}, {2000,1000}};
    SEQBLOCK_STRETCHPOINTS_set(seqblock, only_ends, 2);
    CHECK("num after setting only the endpoints", SEQBLOCK_STRETCHPOINTS_get_num(seqblock), 0);
    CHECK("has after setting only the endpoints", SEQBLOCK_STRETCHPOINTS_has(seqblock), false);

    // Points that are not increasing should be dropped.
    struct SeqBlockStretchPoint bad_points[5] = {
      {0, 0},
      {500, 100},
      {400, 200},   // seqtime decreases
      {600, 50},    // sample pos decreases
      {2000, 1000}
    };
    SEQBLOCK_STRETCHPOINTS_set(seqblock, bad_points, 5);
    CHECK("num with bad points", SEQBLOCK_STRETCHPOINTS_get_num(seqblock), 3);
    CHECK("bad points, sample pos at 500", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 500), 100);
    CHECK("bad points, sample pos at 1000", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 1000), 400);

    // The endpoints are always forced to the start and end of the seqblock.
    struct SeqBlockStretchPoint wrong_ends[3] = {{100, 100}, {500, 500}, {600, 600}};
    SEQBLOCK_STRETCHPOINTS_set(seqblock, wrong_ends, 3);
    CHECK("wrong ends, num", SEQBLOCK_STRETCHPOINTS_get_num(seqblock), 3);
    CHECK("wrong ends, sample pos at 0", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 0), 0);
    CHECK("wrong ends, sample pos at 2000", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 2000), 1000);
  }

  // Drag the point from 400 to 200 (the example from the user: segment 0 gets
  // shorter, the rest is unchanged).
  {
    printf("3. Dragging a stretch point:\n");
    struct SeqBlock *seqblock = create_test_seqblock(2000, 0, 1000, 1.0);

    struct SeqBlockStretchPoint points[4] = {
      {0, 0},
      {400, 250},
      {1200, 600},
      {2000, 1000}
    };
    SEQBLOCK_STRETCHPOINTS_set(seqblock, points, 4);

    CHECK("before drag, sample pos at 400", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 400), 250);

    // Move point 1 (and all following points) 200 frames left. New duration is 1800.
    seqblock->t.time2 = 480000 + 1800;
    seqblock->t.stretch = 1800.0 / 1000.0;
    struct SeqBlockStretchPoint dragged[4] = {
      {0, 0},
      {200, 250},
      {1000, 600},
      {1800, 1000}
    };
    SEQBLOCK_STRETCHPOINTS_set(seqblock, dragged, 4);

    CHECK("after drag, num", SEQBLOCK_STRETCHPOINTS_get_num(seqblock), 4);
    CHECK("after drag, sample pos at 200", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 200), 250);
    CHECK("after drag, sample pos at 1000", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 1000), 600);
    CHECK("after drag, sample pos at 1800", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 1800), 1000);

    // Segment 0 is now 200/250 = 0.8
    double stretch0 = SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 100);
    if (fabs(stretch0 - 0.8) > 0.0001){
      printf("   FAILED: stretch after drag = %f\n", stretch0);
      num_failures++;
    } else
      printf("   ok: stretch after drag = 0.8\n");

    // Segment 1 unchanged: (1000-200)/(600-250) = 800/350
    double stretch1 = SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 500);
    if (fabs(stretch1 - 800.0/350.0) > 0.0001){
      printf("   FAILED: unchanged segment stretch = %f, expected %f\n", stretch1, 800.0/350.0);
      num_failures++;
    } else
      printf("   ok: unchanged segment stretch = %f\n", stretch1);
  }

  // Speed != 1. 1000 frames of audio played at speed 2.0 (one octave up) means
  // the block lasts 500 frames.
  {
    printf("4. Speed 2.0:\n");
    struct SeqBlock *seqblock = create_test_seqblock(500, 0, 1000, 2.0);

    if (fabs(SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 100) - 0.25) > 0.0001){
      printf("   FAILED: stretch at speed 2.0: %f, expected 0.25\n", SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 100));
      num_failures++;
    } else
      printf("   ok: stretch at speed 2.0 = 0.25\n");

    struct SeqBlockStretchPoint points[3] = {
      {0, 0},
      {250, 250},
      {500, 1000}
    };
    SEQBLOCK_STRETCHPOINTS_set(seqblock, points, 3);

    CHECK("speed 2.0, sample pos at 250", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 250), 250);
    CHECK("speed 2.0, sample pos at 500", SEQBLOCK_STRETCHPOINTS_get_sample_pos(seqblock, 500), 1000);

    // Segment 0: 250/(250-0) / 2.0 = 0.5
    double stretch0 = SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 100);
    if (fabs(stretch0 - 0.5) > 0.0001){
      printf("   FAILED: speed 2.0 segment 0: %f, expected 0.5\n", stretch0);
      num_failures++;
    } else
      printf("   ok: speed 2.0 segment 0 = 0.5\n");

    // Segment 1: 250/(1000-250) / 2.0 = 0.16666
    double stretch1 = SEQBLOCK_STRETCHPOINTS_get_stretch(seqblock, 400);
    if (fabs(stretch1 - (250.0/750.0)/2.0) > 0.0001){
      printf("   FAILED: speed 2.0 segment 1: %f\n", stretch1);
      num_failures++;
    } else
      printf("   ok: speed 2.0 segment 1 = %f\n", stretch1);
  }

  // duration_changed
  {
    printf("5. duration_changed:\n");
    struct SeqBlock *seqblock = create_test_seqblock(2000, 0, 1000, 1.0);

    struct SeqBlockStretchPoint points[3] = {{0, 0}, {400, 250}, {2000, 1000}};
    SEQBLOCK_STRETCHPOINTS_set(seqblock, points, 3);

    seqblock->t.time2 = 480000 + 1000;
    SEQBLOCK_STRETCHPOINTS_duration_changed(seqblock, 2000, 1000);

    int64_t seqtime = 0;
    int64_t sample_pos = 0;
    SEQBLOCK_STRETCHPOINTS_get(seqblock, 1, &seqtime, &sample_pos);
    CHECK("point seqtime after halving the duration", seqtime, 200);
    CHECK("point sample pos after halving the duration", sample_pos, 250);
    CHECK("last point seqtime", (SEQBLOCK_STRETCHPOINTS_get(seqblock, 2, &seqtime, &sample_pos) ? seqtime : -1), 1000);
  }

  printf("\n%s (%d failures)\n", num_failures==0 ? "ALL TESTS SUCCEEDED" : "FAILURES", num_failures);

  return num_failures==0 ? 0 : 1;
}
