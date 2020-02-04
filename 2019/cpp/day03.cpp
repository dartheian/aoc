#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cmath>

using namespace std;

typedef pair<char, int>    Vector;
typedef pair<int,  int>    Point;
typedef vector<Point>      Curve;
typedef pair<Point, Point> Segment;
typedef pair<int,  int>    Range;
typedef pair<Range, Range> Projection;

Vector parse_vector(const string & input)
{
    istringstream stream {input};

    char direction;
    stream >> direction;

    int magitude;
    stream >> magitude;

    return Vector {direction, magitude};
}

vector<Vector> parse_vectors(const string & input)
{
    vector<Vector> vectors;

    istringstream stream {input};
    string vector;

    while(getline(stream, vector, ',')) vectors.push_back(parse_vector(vector));

    return vectors;
}

Curve generate_segments(const vector<Vector> & vectors)
{
    Curve curve;

    int x {0}, y {0};

    for(const pair<char, int> & v : vectors)
    {
        switch(v.first)
        {
            case 'U': y += v.second; break;
            case 'D': y -= v.second; break;
            case 'L': x -= v.second; break;
            case 'R': x += v.second; break;
        }

        curve.push_back(Point {x, y});
    }

    return curve;
}

Projection project_segment(const Point & a, const Point & b)
{
    Range x_range {min(a.first, b.first), max(a.first, b.first)};
    Range y_range {min(a.second, b.second), max(a.second, b.second)};
    return Projection {x_range, y_range};
}

vector<Projection> project_all_segments(const Curve & curve)
{
    vector<Projection> projection {curve.size() - 1};
    transform(curve.begin(), curve.end(), ++curve.begin(), projection.begin(), project_segment);
    return projection;
}

pair<vector<Projection>, vector<Projection>> load_projections(const string & filename)
{
    ifstream file {filename};

    if(!file.is_open()) throw runtime_error {"error while opening file"};

    pair<vector<Projection>, vector<Projection>> wires;
    string wire;

    getline(file, wire);
    wires.first = project_all_segments(generate_segments(parse_vectors(wire)));

    getline(file, wire);
    wires.second = project_all_segments(generate_segments(parse_vectors(wire)));

    if(file.bad()) throw runtime_error {"error while reading file"};

    return wires;
}

bool do_range_intersect(Range range1, Range range2)
{
    return !(range1.second < range2.first || range2.second < range1.first);
}

bool do_projection_intersect(Projection p1, Projection p2)
{
    return do_range_intersect(p1.first, p2.first) && do_range_intersect(p1.second, p2.second);
}

Range intersect_range(Range range1, Range range2)
{
    return Range {max(range1.first, range2.first), min(range1.second, range2.second)};
}

Projection intersect_projection(Projection p1, Projection p2)
{
    return Projection {intersect_range(p1.first, p2.first), intersect_range(p1.second, p2.second)};
}

int compute_projection_distance(Projection p)
{
    if(p.first == Range {0, 0} && p.second == Range {0, 0}) return 0;

    bool x_contains_0 = do_range_intersect(p.first,  Range {0, 0});
    bool y_contains_0 = do_range_intersect(p.second, Range {0, 0});

    if(x_contains_0 && y_contains_0) return 1;

    int x = x_contains_0 ? 0 : min(abs(p.first.first),  abs(p.first.second));
    int y = y_contains_0 ? 0 : min(abs(p.second.first), abs(p.second.second));

    return x + y;
}

// Projection intersect_segment(Segment s1, Segment s2)
// {
//     Projection p1 = project_segment(s1);
//     Projection p2 = project_segment(s2);

//     return do_projection_intersect(p1, p2) ? intersect_projection(p1, p2) : Projection {0,0};
// }

// int search_nearest_intersection(vector<Projection> p1, vector<Projection> )
// {
// }

int main(const int argc, char const * const argv[])
{
    if(argc != 2) throw invalid_argument {"missing input filename"};

    pair<vector<Projection>, vector<Projection>> wires {load_projections(argv[0])};
}