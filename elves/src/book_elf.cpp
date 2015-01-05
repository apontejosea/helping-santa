#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
double CalculateN(double t) {
  if (!(t >= 0))
    exit(1);
  if(0  <= t & t < 9)
    return 0;
  else if(9  <= t & t < 19)
    return (t-9);
  else
    return 10;
}

// [[Rcpp::export]]
double CalculateM(double t) {
  if (!(t >= 0)) exit(1);
  if(0  <= t & t < 9)
    return t;
  else if(9  <= t & t < 19)
    return 9;
  else
    return t-10;
}

// [[Rcpp::export]]
Datetime DateTimeAt900(Datetime date_time) {
  double orig_time;
  // BUG?: what about seconds? Would this be related to round-off differences?
  // original time portion of date_time in seconds
  orig_time = date_time.getHours()*60*60 + date_time.getMinutes()*60 + date_time.getSeconds();
  return (date_time - orig_time + 9*60*60);
}

// [[Rcpp::export]]
Datetime DateTimeNext900(Datetime dtime) {
    Datetime beg_of_workday;
    Datetime result;
    double orig_time;
    int addition;
    
    orig_time      = dtime.getHours()*60*60 + dtime.getMinutes()*60 + dtime.getSeconds();
    beg_of_workday = dtime - orig_time;
    addition       = 9*60*60;
    if(dtime.getHours() > 9) {
        addition = addition + 24*60*60;
    }
    result         = beg_of_workday + addition;
    return result;
}

// [[Rcpp::export]]
Datetime DateTimeAt1900(Datetime date_time) {
  double orig_time;
  // added original time portion of date_time in seconds
  orig_time = date_time.getHours()*60*60 + date_time.getMinutes()*60 + date_time.getSeconds();
  return (date_time - orig_time + 19*60*60);
}

// [[Rcpp::export]]
double CalculateDaysDifference(Datetime start, Datetime end) {
  Datetime new_end, new_start;
  double days, origEndTime, origStartTime;

  days = 0;
  //remove time portion
  origEndTime   = end.getHours()*60 + end.getMinutes() + end.getSeconds()/60; //added seconds
  new_end       = end - origEndTime*60;
  origStartTime = start.getHours()*60 + start.getMinutes() + start.getSeconds()/60;
  new_start     = start - origStartTime*60;
  days          = (new_end - new_start)/60.0/60.0/24.0;
  return days;
}

// [[Rcpp::export]]
double CalcHours(Datetime date_time) {
  return date_time.getHours() + date_time.getMinutes()/60.0 + date_time.getSeconds()/60.0/60;//added seconds
}

// [[Rcpp::export]]
double CalculateSanctionedHours(Datetime start, double duration) {
  Datetime expected_end;
  double expected_end_hr,start_hr,sanctioned_hours;
  
  expected_end = start + duration*60;
  expected_end_hr = CalcHours(expected_end);
  start_hr = CalcHours(start);
  sanctioned_hours = CalculateN(CalcHours(expected_end)) - 
  CalculateN(CalcHours(start)) +
  CalculateDaysDifference(start, expected_end)*10;
  return sanctioned_hours;
}

// [[Rcpp::export]]
double CalculateUnsanctionedHours(Datetime start, double duration) {
  Datetime expected_end;
  double expected_end_hr, start_hr, unsanctioned_hours;
  
  expected_end       = start + duration*60;
  expected_end_hr    = CalcHours(expected_end);
  start_hr           = CalcHours(start);
  unsanctioned_hours = CalculateM(expected_end_hr) - CalculateM(start_hr) +
  CalculateDaysDifference(start, expected_end)*14;
  return unsanctioned_hours;
}

// [[Rcpp::export]]
double CalculateP(double p0,double n,double m) {
  double p;
  p = std::min(4.0,std::max(0.25, p0*(pow(1.02,n)*pow(0.9,m))));
  return p;
}

// [[Rcpp::export]]
Datetime NextSanctionedDateTime(Datetime date_time) {
  if(9 <= CalcHours(date_time) & CalcHours(date_time) < 19) {
    return date_time;
  }
  else {
    return DateTimeNext900(date_time);
  }
}

// [[Rcpp::export]]
Datetime CalculateDateTimeAfterResting(Datetime previous_end, double rest_period) {
  Datetime datetime_after_resting;
  datetime_after_resting = NextSanctionedDateTime(previous_end) +
                           ((int)(rest_period/10))*24*60*60 +
                           (fmod(rest_period, 10.0)*60.0*60.0);
  return NextSanctionedDateTime(datetime_after_resting);
}

// [[Rcpp::export]]
double CalculateElfDurationMinutes(double duration, double productivity) {
  return std::ceil(duration/productivity);
}

// [[Rcpp::export]]
Datetime PickStart(Datetime previous_start, Datetime previous_end, 
                   int duration, double productivity) {

  Datetime s_e, s_a, previous_start_1900, temp_date_time;
  double rest_time_hr;

  previous_start_1900 = DateTimeAt1900(previous_start);
  
  rest_time_hr = std::max(0.0, (previous_end - previous_start_1900)/60/60);
  s_e          = CalculateDateTimeAfterResting(previous_end, rest_time_hr)+60;

  if(CalculateUnsanctionedHours(s_e , duration) > 0) {
    s_e        = DateTimeNext900(s_e);
  }

  return s_e;
}
