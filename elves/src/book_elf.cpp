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
  // BUG?: what about seconds? Would this be related to round-off differences?
  // added original time portion of date_time in seconds
  orig_time = date_time.getHours()*60*60 + date_time.getMinutes()*60 + date_time.getSeconds();
  return (date_time - orig_time + 19*60*60);
}

//does this work like this?
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
  CalculateDaysDifference(start, expected_end)*10; //revise CalculateDaysDifference()
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
  CalculateDaysDifference(start, expected_end)*14; //revise CalculateDaysDifference()
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
  if(9 <= CalcHours(date_time) & CalcHours(date_time) <= 19) {
    return date_time;
  }
  else {
    return DateTimeNext900(date_time);
  }
}


// [[Rcpp::export]]
Datetime CalculateDateTimeAfterResting2(Datetime previous_end, double rest_period) {
    Datetime datetime_after_resting,temp_next_sanctioned_datetime;
  
    if(9 <= CalcHours(previous_end) & CalcHours(previous_end) <= 19) {
        temp_next_sanctioned_datetime = previous_end + 60;
    }
    else {
        temp_next_sanctioned_datetime = DateTimeNext900(previous_end);
    }
  
    datetime_after_resting = temp_next_sanctioned_datetime +
                             ((int)(rest_period/10))*24*60*60 +
                             (fmod(rest_period, 10.0)*60.0*60.0);
    return NextSanctionedDateTime(datetime_after_resting);
}


// [[Rcpp::export]]
Datetime PickStart(Datetime previous_start, Datetime previous_end, 
                    int duration, double productivity, double threshold) {

  Datetime s_e, s_a, previous_start_1900, temp_date_time;
  double rest_time_hr;
  //double   rest_time_hr, n_a, m_a, p_a, n_e, m_e, p_e, advantage_index;
  
  previous_start_1900 = DateTimeAt1900(previous_start);
  
  rest_time_hr = std::max(0.0, (previous_end - previous_start_1900)/60/60);
  s_e          = CalculateDateTimeAfterResting2(previous_end, rest_time_hr);
    return s_e;

 /* s_a          = DateTimeAt900(s_e);
  s_a          = DateTimeNext900(s_e);
  n_e          = CalculateSanctionedHours(s_e, duration);
  n_a          = CalculateSanctionedHours(s_a, duration);
  m_e          = CalculateUnsanctionedHours(s_e, duration);
  m_a          = CalculateUnsanctionedHours(s_a, duration);
  p_e          = CalculateP(productivity, n_e, m_e);
  p_a          = CalculateP(productivity, n_a, m_a);
 
  advantage_index = (p_a - p_e)*((s_a - s_e)/60.0/60.0);

  //std::cout << "advantage index: " << advantage_index <<std::endl;
  if(advantage_index > threshold) {
    return(s_a);
  }
  else {
    return(s_e);
  }*/
}


// [[Rcpp::export]]
DataFrame BookElf(DatetimeVector& arrival, NumericVector& duration, double threshold) {
 
  //variable declarations
  int length = arrival.size();
  int current_duration;
  NumericVector  v_p(length), v_n(length), v_m(length),v_duration_out(length);
  DatetimeVector v_start(length), v_end(length);
  Datetime       earliest_start;
  
  // initialize first entry of vectors
  v_p[0]            = 1;
  v_start[0]        = NextSanctionedDateTime(arrival[0]);
  v_end[0]          = v_start[0] + std::ceil(duration[0]/v_p[0])*60;
  v_n[0]            = CalculateSanctionedHours(v_start[0], duration[0]);
  v_m[0]            = CalculateUnsanctionedHours(v_start[0], duration[0]);
  v_duration_out[0] = std::ceil(duration[0]/v_p[0]);
 
  // TODO: m is turning out to be zero...
  for(int i=1; i<length; i++) {
    v_p[i]            = CalculateP(v_p[i-1], v_n[i-1], v_m[i-1]);
    earliest_start    = PickStart(v_start[i-1], v_end[i-1], duration[i],
                                v_p[i], threshold);
    v_start[i]        = std::max(earliest_start,NextSanctionedDateTime(arrival[i]));
    current_duration  = std::ceil(duration[i]/v_p[i]);
    v_end[i]          = v_start[i] + current_duration*60;
    v_n[i]            = CalculateSanctionedHours(v_start[i], current_duration);
    v_m[i]            = CalculateUnsanctionedHours(v_start[i], current_duration);
    v_duration_out[i] = current_duration;
  }

  return DataFrame::create(_["p"]  = v_p,_["start"]= v_start, 
                           _["end"]= v_end,_["n"]  = v_n,
                           _["m"]  = v_m,_["duration"]= v_duration_out);
}

//DataFrame BookElf(DatetimeVector& arrival, NumericVector& duration, 
//                  double threshold) {
// 
//  //variable declarations
//  int length = arrival.size();
//  NumericVector  v_p(length), v_rest(length), v_n(length), v_m(length);
//  DatetimeVector v_start(length), v_end(length);
//  Datetime       earliest_start;
//  
//  // initialize first entry of vectors
//  v_p[0]           = 1;
//  v_rest[0]        = 0;
//  earliest_start   = NextSanctionedDateTime(arrival[0]);
//  v_start[0]       = earliest_start;
//  v_end[0]         = v_start[0] + duration[0]/v_p[0]*60;
//  v_n[0]           = CalculateSanctionedHours(v_start[0], duration[0]);
//  v_m[0]           = CalculateUnsanctionedHours(v_start[0], duration[0]);
//
//  // m is turning out to be zero...
//  for (int i=1; i<length; i++) {
//    v_p[i]         = CalculateP(v_p[i-1],v_n[i-1],v_m[i-1]);
//    v_rest[i]      = CalculateUnsanctionedHours(v_start[i-1], duration[i-1]/v_p[i-1]);
//    earliest_start = CalculateDateTimeAfterResting(v_end[i-1], v_rest[i]);
//    v_start[i]     = PickStart(earliest_start, duration[i], v_p[i], threshold);
//    v_end[i]       = v_start[i] + duration[i]/v_p[i]*60;
//    v_n[i]         = CalculateSanctionedHours(v_start[i], duration[i]);
//    v_m[i]         = CalculateUnsanctionedHours(v_start[i], duration[i]);
//  }
//
//  return DataFrame::create(_["p"]= v_p, _["start"]= v_start, 
//                           _["end"]= v_end,_["n"]= v_n,
//                           _["m"]= v_m,_["rest"]= v_rest);
//}


//Datetime PickStart5_previous(Datetime previous_start, Datetime previous_end, 
//                    int duration, double productivity, double threshold) {
//
//  Datetime s_e, s_a, previous_start_1900, temp_date_time;
//  double   rest_time_hr, n_a, m_a, p_a, n_e, m_e, p_e, advantage_index;
//  
//  previous_start_1900 = DateTimeAt1900(previous_start);
//    
//  rest_time_hr = std::max(0.0, (previous_end - previous_start_1900)/60/60);
//  s_e          = CalculateDateTimeAfterResting(previous_end, rest_time_hr);
//  s_a          = DateTimeAt900(s_e);
//  n_e          = CalculateSanctionedHours(s_e, duration);
//  n_a          = CalculateSanctionedHours(s_a, duration);
//  m_e          = CalculateUnsanctionedHours(s_e, duration);
//  m_a          = CalculateUnsanctionedHours(s_a, duration);
//  p_e          = CalculateP(productivity, n_e, m_e);
//  p_a          = CalculateP(productivity, n_a, m_a);
// 
//  advantage_index = (p_a - p_e)*((s_a - s_e)/60.0/60.0);
//
//  //std::cout << "advantage index: " << advantage_index <<std::endl;
//  if(advantage_index > threshold) {
//    return(s_a);
//  }
//  else {
//    return(s_e);
//  }
//}


//Datetime PickStart(Datetime earliest_start, int duration, 
//                   double productivity, double threshold) {
//    Datetime s_e, s_a;
//    double n_e, n_a, m_e, m_a, p_e, p_a, advantage_index;
//
//    s_e = earliest_start;
//    s_a = std::max(s_e, DateTimeNext900(s_e));
//
//    n_e = CalculateSanctionedHours(s_e, duration);
//    n_a = CalculateSanctionedHours(s_a, duration);
//    m_e = CalculateUnsanctionedHours(s_e, duration);
//    m_a = CalculateUnsanctionedHours(s_a, duration);
//    p_e = CalculateP(productivity, n_e, m_e);
//    p_a = CalculateP(productivity, n_a, m_a);
//
//    advantage_index = (p_a - p_e)*((s_a - s_e)/60.0/60.0);
//    //std::cout << "advantage index: " << advantage_index <<std::endl;
//
//    if(advantage_index > threshold) {
//      return s_a;
//    }
//    else {
//      return s_e;
//    }
//}

// [[Rcpp::export]]
//Datetime CalculateDateTimeAfterResting(Datetime previous_end, double rest_period) {
//  Datetime datetime_after_resting;
//  datetime_after_resting = NextSanctionedDateTime(previous_end) +
//                           ((int)(rest_period/10))*24*60*60 +
//                           (fmod(rest_period, 10.0)*60.0*60.0);
//  return NextSanctionedDateTime(datetime_after_resting);
//}


