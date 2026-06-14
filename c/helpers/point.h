#ifndef HELPERS_POINT_H
#define HELPERS_POINT_H

typedef struct {
    int x;
    int y;
} point_t;

static inline point_t add_point(point_t a, point_t b)
{
    return (point_t){ a.x + b.x, a.y + b.y };
}

#endif // HELPERS_POINT_H
