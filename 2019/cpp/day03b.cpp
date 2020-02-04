#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace std;

typedef pair<char,  int>   Step;
typedef pair<int,   int>   Point;
typedef pair<Point, Point> Segment;

Step parse_step(const string & input)
{
    istringstream stream {input};

    char direction;
    stream >> direction;

    int magitude;
    stream >> magitude;

    return Step {direction, magitude};
}

vector<Step> parse_steps(const string & input)
{
    vector<Step> steps;

    istringstream stream {input};
    string step;

    while(getline(stream, step, ',')) steps.push_back(parse_step(step));

    return steps;
}

vector<Point> generate_points(const vector<Step> & steps)
{
    vector<Point> points;

    int x {0}, y {0};

    for(const Step & step : steps)
    {
        switch(step.first)
        {
            case 'U': y += step.second; break;
            case 'D': y -= step.second; break;
            case 'L': x -= step.second; break;
            case 'R': x += step.second; break;
        }

        points.push_back(Point {x, y});
    }

    return points;
}

pair<vector<Point>, vector<Point>> load_wires(const string & filename)
{
    ifstream file {filename};

    if(!file.is_open()) throw runtime_error {"error while opening file"};

    pair<vector<Point>, vector<Point>> wires;
    string wire;

    getline(file, wire);
    wires.first = generate_points(parse_steps(wire));

    getline(file, wire);
    wires.second = generate_points(parse_steps(wire));

    if(file.bad()) throw runtime_error {"error while reading file"};

    return wires;
}

Segment order_points(const Segment & segment)
{
    Point a {segment.first};
    Point b {segment.second};

    Point p {min(a.first, b.first), min(a.second, b.second)};
    Point q {max(a.first, b.first), max(a.second, b.second)};

    return Segment {p, q};
}

bool horizontal(const Segment & segment)
{
    Point a {segment.first};
    Point b {segment.second};

    return a.second == b.second;
}

bool between(const int a, const int b, const int c)
{
    return b <= a && a <= c;
}

int manhattan_distance(Point point)
{
    return abs(point.first) + abs(point.second);
}

int intersection_distance(const Segment & s1, const Segment & s2)
{
    int x1 {s1.first.first};
    int x2 {s1.second.first};
    int x3 {s2.first.first};
    int y1 {s1.first.second};
    int y3 {s2.first.second};
    int y4 {s2.second.second};

    if(between(x3, x1, x2) && between(y1, y3, y4)) return manhattan_distance(Point {x3, y1});

    return 0;
}

int intersect(const Segment & s1, const Segment & s2)
{
    Segment a {order_points(s1)};
    Segment b {order_points(s2)};

    bool h1 {horizontal(a)};
    bool h2 {horizontal(b)};

    if(h1 && !h2) return intersection_distance(a, b);
    if(h2 && !h1) return intersection_distance(b, a);

    return 0;
}

int find_nearest_intersection(vector<Point> wire1, vector<Point> wire2)
{
    auto i1 {wire1.begin()}, end1 {--wire1.end()};
    auto i2 {wire2.begin()}, end2 {--wire2.end()};

    int res {numeric_limits<int>::max()};

    while(i1 != end1)
    {
        Segment s1 {*i1, *(i1+1)};

        while(i2 != end2)
        {
            Segment s2 {*i2, *(i2+1)};

            int distance {intersect(s1, s2)};

            if(distance != 0 && distance < res) res = distance;

            ++i2;
        }

        ++i1;
    }

    return res;
}

int main(const int argc, char const * const argv[])
{
    if(argc != 2) throw invalid_argument {"missing input filename"};

    pair<vector<Point>, vector<Point>> wires {load_wires(argv[1])};

    vector<Point> wire1 {wires.first};
    vector<Point> wire2 {wires.second};

    cout << find_nearest_intersection(wire1, wire2) << endl;
}