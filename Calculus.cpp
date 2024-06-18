#include <iostream>
#include <map>
#include <string>
#include <tuple>
#include <vector>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <cmath>
using namespace std;

#define E 2.71828182845904523536
#define PI 3.14159265358979323846

class Calculator {
private:
    map<string, int> smap;
    unordered_map<string, tuple<double, string>> vars;

    bool containsLetter(const string& str) {
        return any_of(str.begin(), str.end(), [](char c) {
            return isalpha(static_cast<unsigned char>(c));
        });
    }

    bool isNumber(const string& str) {
        regex number_regex("^-?\\d*(\\.\\d+)?$");
        return regex_match(str, number_regex);
    }

    bool canBeConvertedTodouble(const string& str) {
        try {
            stof(str);
            return true;
        } catch (const invalid_argument& e) {
            return false;
        } catch (const out_of_range& e) {
            return false;
        }
    }

    double operators(bool functions, vector<int> ncount, vector<int> states, string s, string funcstr,
                     vector<double> nums, vector<string> numvars) {
        if (s.find("+=") != string::npos) {
            funcstr = s.substr(0, s.find("+="));
            s = s.substr(s.find('+=') + 1);
            get<0>(vars[funcstr]) += operators(false, ncount, states, s, funcstr, nums, numvars);
            get<1>(vars[funcstr]) = to_string(get<0>(vars[funcstr]));
            return get<0>(vars[funcstr]);
        }

        if (s.find('=') != string::npos) {
            functions = true;
            funcstr = s.substr(0, s.find('='));
            s = s.substr(s.find('=') + 1);
            get<1>(vars[funcstr]) = s;

            if (canBeConvertedTodouble(s)) {
                get<0>(vars[funcstr]) = stof(s);
                get<1>(vars[funcstr]) = s;
                return stof(s);
            }
        } else if (canBeConvertedTodouble(s)) {
            return stof(s);
        } else if (s.find('=') == string::npos && s.find('/') == string::npos && s.find('(') == string::npos) {
            return operators(false, ncount, states, "(+ 0 " + s + ")", funcstr, nums, numvars);
        }

        bool read = true;
        string strbuild;
        string intbuild;
        bool stract = true;

        for (size_t i = 0; i < s.length(); ++i) {
            if (isspace(s[i]) || s[i] == ')') {
                if (!strbuild.empty() && stract) {
                    states.push_back(smap[strbuild]);
                    ncount.push_back(0);
                    stract = false;
                }
                if (!intbuild.empty()) {
                    ncount.back()++;
                    if (intbuild == "e") {
                        nums.push_back(E);
                    } else if (intbuild == "pi") {
                        nums.push_back(PI);
                    } else if (vars.find(intbuild) != vars.end() && !containsLetter(get<1>(vars[intbuild]))) {
                        nums.push_back(get<0>(vars[intbuild]));
                    } else if (vars.find(intbuild) != vars.end() && containsLetter(get<1>(vars[intbuild]))) {
                        nums.push_back(operators(false, {}, states, get<1>(vars[intbuild]), "", nums, {}));
                    } else {
                        nums.push_back(stof(intbuild));
                    }
                    numvars.push_back(intbuild);
                    intbuild.clear();
                }
                if (s[i] == ')') {
                    if (nums.size() >= 2 && ncount.back() >= 2) {
                        ncount.pop_back();
                        checker(nums, states, ncount, numvars);
                        if (!ncount.empty()) {
                            ncount.back()++;
                        }
                    }
                }
                strbuild.clear();
                read = false;
            } else if (s[i] == '(') {
                read = true;
                stract = true;
                strbuild.clear();
            } else if (s[i] != '(' && s[i] != ')' && s[i] != ' ' && read) {
                strbuild += s[i];
            } else if (s[i] != '(' && s[i] != ')' && s[i] != ' ' && !read) {
                intbuild += s[i];
            }
        }

        if (!intbuild.empty()) {
            nums.push_back(stof(intbuild));
        }

        if (!nums.empty()) {
            if (functions) {
                get<0>(vars[funcstr]) = nums.back();
            }
            return nums.back();
        }

        nums.clear();
        states.clear();
        return 0.0;
    }

    void checker(vector<double>& nums, vector<int>& states, vector<int> ncount, vector<string>& numvars) {
        if (nums.size() < 2) {
            return;
        }
        double a = nums[nums.size() - 2];
        double b = nums[nums.size() - 1];
        double y = 0;

        switch (states.back()) {
            case 0:
                y = a + b;
                break;
            case 1:
                y = a - b;
                break;
            case 2:
                y = a * b;
                break;
            case 3:
                y = a / b;
                break;
            case 4:
                y = pow(a, b);
                break;
            case 5:
                y = log(b) / log(a);
                break;
            case 6:
                y = sin(a * b);
                break;
            case 7:
                y = cos(a * b);
                break;
            case 8:
                for (double i = nums[nums.size() - 3]; i <= nums[nums.size() - 2]; i += nums[nums.size() - 1]) {
                    operators(false, ncount, states, numvars[numvars.size() - 4] + "= " + to_string(i), "", nums, numvars);
                    y += operators(false, ncount, states, numvars[numvars.size() - 5], "", nums, numvars);
                }
                numvars.pop_back();
                numvars.pop_back();
                numvars.pop_back();
                nums.pop_back();
                nums.pop_back();
                nums.pop_back();
                break;
            case 9:
                operators(false, ncount, states, numvars[numvars.size() - 1] + "+=0.000001", "", nums, numvars);
                double d = operators(false, ncount, states, numvars[numvars.size() - 2], "", nums, numvars);
                operators(false, ncount, states, numvars[numvars.size() - 1] + "+=-0.000001", "", nums, numvars);
                y = (d - a) / 0.000001f;
                break;
        }
        nums.pop_back();
        nums.pop_back();
        numvars.pop_back();
        numvars.pop_back();
        nums.push_back(y);
        numvars.push_back(to_string(y));
        states.pop_back();
    }

public:
    Calculator() {
        smap = {
                {"+", 0},
                {"-", 1},
                {"*", 2},
                {"/", 3},
                {"^", 4},
                {"log", 5},
                {"sin", 6},
                {"cos", 7},
                {"sum", 8},
                {"deriv", 9},
        };
    }

    double calculate(const string& s) {
        bool functions = false;
        string funcstr;
        vector<int> ncount;
        vector<int> states;
        vector<double> nums;
        vector<string> numvars;

        int line_count = 0;
        for (char c : s) {
            if (c == '\n') {
                line_count++;
            }
        }
        line_count++;

        stringstream ss(s);
        string line;
        int current_line = 0;



        try {
            while (getline(ss, line, '\n')) {
                current_line++;
                if (current_line == line_count) {

                    return operators(functions, ncount, states, line, funcstr, nums, numvars);
                } else {
                    operators(functions, ncount, states, line, funcstr, nums, numvars);
                }
            }
        } catch (const exception& e) {
            cerr << "Invalid operation: " << e.what() << endl;
            return 0.0;
        }
    }
};
