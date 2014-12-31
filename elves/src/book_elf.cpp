#include <Rcpp.h>
#include <queue>
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
Datetime CalculateDateTimeAfterResting(Datetime previous_end, double rest_period) {
    Datetime datetime_after_resting;
    datetime_after_resting = NextSanctionedDateTime(previous_end+60) +
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
    s_e          = CalculateDateTimeAfterResting(previous_end, rest_time_hr);
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
double CalculateElfDurationMinutes(double duration, double productivity) {
    return std::ceil(duration/productivity);
}

// [[Rcpp::export]]
DataFrame BookElf(DatetimeVector& arrival, NumericVector& duration, double threshold) {
    
    //variable declarations
    int length = arrival.size();
    NumericVector  v_p(length), v_n(length), v_m(length),v_elf_duation(length);
    DatetimeVector v_start(length), v_end(length);
    Datetime       earliest_start;
    
    // initialize first entry of vectors
    v_p[0]            = 1;
    v_elf_duation[0]  = CalculateElfDurationMinutes(duration[0], v_p[0]);
    v_start[0]        = NextSanctionedDateTime(arrival[0]);
    v_end[0]          = v_start[0] + CalculateElfDurationMinutes(duration[0], v_p[0])*60;
    v_n[0]            = CalculateSanctionedHours(v_start[0], duration[0]);
    v_m[0]            = CalculateUnsanctionedHours(v_start[0], duration[0]);
    
    for(int i=1; i<length; i++) {
        v_p[i]            = CalculateP(v_p[i-1], v_n[i-1], v_m[i-1]);
        earliest_start    = PickStart(v_start[i-1], v_end[i-1], duration[i], v_p[i], threshold);
        v_start[i]        = std::max(earliest_start, NextSanctionedDateTime(arrival[i]));
        v_elf_duation[i]  = CalculateElfDurationMinutes(duration[i], v_p[i]);
        v_end[i]          = v_start[i] + v_elf_duation[i]*60;
        v_n[i]            = CalculateSanctionedHours(v_start[i], v_elf_duation[i]);
        v_m[i]            = CalculateUnsanctionedHours(v_start[i], v_elf_duation[i]);
    }
    
    return DataFrame::create(_["p"]  = v_p,_["start"]= v_start,
                             _["end"]= v_end,_["n"]  = v_n,
                             _["m"]  = v_m,_["duration"]= v_elf_duation);
}


// [[Rcpp::export]]
DataFrame MutationByP2(NumericVector& elf_id, NumericVector& toy_id, NumericVector& p, NumericVector& duration_in, DatetimeVector& arrival)
{
    int i = 0;
    int j = 0;
    int index = 0;
    int elf_num = 0;
    int last_index = 0;
    
    int temp_duration, temp_elf_id,temp_toy_id;
    
    NumericVector  v_elf_id(10000000), v_toy_id(10000000), v_p(10000000), v_duration_in(10000000);
    DatetimeVector v_arrival(10000000);
    Datetime temp_arrival;
    
    v_elf_id      = elf_id;
    v_toy_id      = toy_id;
    v_arrival     = arrival;
    v_p           = p;
    v_duration_in = duration_in;
    
    
    for (elf_num=1; elf_num<=900; elf_num++){
        //std::cout<<"for loop"<<std::endl;
        while (elf_num == v_elf_id[index]) {
            index++;
        }
        last_index = index-1;
        //std::cout<<last_index<<std::endl;
        
        while (i<last_index & j<last_index){
            //std::cout<<"second while"<<std::endl;
            if (v_p[i] == 4) {
                //find next i with big p decrease
                j = i+1;
                while (v_p[j]>.25 & j<last_index){
                    j++;
                }//while
                
                //swap values
                temp_arrival        = v_arrival[i];
                temp_duration       = v_duration_in[i];
                temp_elf_id         = v_elf_id[i];
                temp_toy_id         = v_toy_id[i];
                
                v_arrival[i]        = v_arrival[j];
                v_duration_in[i]    = v_duration_in[j];
                v_elf_id[i]         = v_elf_id[j];
                v_toy_id[i]         = v_toy_id[j];
                
                v_arrival[j]        = temp_arrival;
                v_duration_in[j]    = temp_duration;
                v_elf_id[j]         = temp_elf_id;
                v_toy_id[j]         = temp_toy_id;
                /*
                 std::cout<<"old i: "<<i<<std::endl;
                 std::cout<<"old i's duration: "<<temp_duration<<std::endl;
                 std::cout<<"old j: "<<j<<std::endl;
                 std::cout<<"old j's duration:"<<v_duration_in[i]<<std::endl; */
                
                
                //update i
                
                v_p[i] = 0;
                //i = j;
                i=j+(std::rand() % 100-50) + 50;//trying a little separation
            }//if
            else{
                i++;
            }//else
            
        }//while
        /*
         std::cout<<"i: "<<i<<std::endl;
         std::cout<<"j: "<<j<<std::endl;
         std::cout<<"index: "<<index<<std::endl;
         std::cout<<"last index: "<<last_index<<std::endl;
         std::cout<<"elf_num: "<<elf_num<<std::endl;
         std::cout<<"v_elf_num: "<<v_elf_id[index]<<std::endl; */
        i=last_index + 1;
    }//for
    
    return DataFrame::create(_["ElfId"]  = v_elf_id,_["ToyId"]= v_toy_id,
                             _["Arrival"]= v_arrival,_["Duration"]  = v_duration_in);
    
}//DataFrame


// [[Rcpp::export]]
DataFrame SortToys2(DatetimeVector& arrival, NumericVector& duration_in, NumericVector& big_toy,int& sep_factor){
    

    int start_index = 0;
    int counter = 0;
    int current_index = 0;
    int small_toy_counter = 0;
    int length = arrival.size();
    
    NumericVector   v_duration_in(length),v_big_toy(length);
    DatetimeVector v_arrival(length);
    
    std::queue <int> q_big_toy_index;
    std::queue <int> q_small_toy_index;
    

        //save index in corresponding queue
        while (start_index<length){
            if (big_toy[start_index]==0){
                q_small_toy_index.push(start_index);
                start_index++;
            }
            else{
                q_big_toy_index.push(start_index);
                start_index++;
            }
        }
    
        std::cout<<"Total toys: "<< q_small_toy_index.size() <<" + "<< q_big_toy_index.size()<<" = "<<q_small_toy_index.size() + q_big_toy_index.size()<<std::endl;
        
        while (counter<length){
            if (q_small_toy_index.size()>=sep_factor & q_big_toy_index.size()>=1) {
                while (small_toy_counter<sep_factor) {
                    current_index           = q_small_toy_index.front();
                    q_small_toy_index.pop();
                    v_arrival[counter]     = arrival[current_index];
                    v_duration_in[counter] = duration_in[current_index];
                    v_big_toy[counter]     = big_toy[current_index];
                    small_toy_counter      = small_toy_counter + 1;
                    counter                = counter + 1;
                }
                //time to add big toy and reset small counter
                current_index           = q_big_toy_index.front();
                q_big_toy_index.pop();
                v_arrival[counter]     = arrival[current_index];
                v_duration_in[counter] = duration_in[current_index];
                v_big_toy[counter]     = big_toy[current_index];
                counter = counter + 1;
                small_toy_counter = 0;
            }
            //if small toys left append them to the end
            else if(q_small_toy_index.size()>=1){
                while (q_small_toy_index.size()!=0) {
                    current_index          = q_small_toy_index.front();
                    q_small_toy_index.pop();
                    v_arrival[counter]     = arrival[current_index];
                    v_duration_in[counter] = duration_in[current_index];
                    v_big_toy[counter]     = big_toy[current_index];
                    counter                = counter + 1;
                }
                std::cout<<"We had small toys left on elf. "<<std::endl;
            }
            //else if only big toys left append them to the end and output warning to avoid this in the future
            else {
                while (q_big_toy_index.size()!=0) {
                    current_index          = q_big_toy_index.front();
                    q_big_toy_index.pop();
                    v_arrival[counter]     = arrival[current_index];
                    v_duration_in[counter] = duration_in[current_index];
                    v_big_toy[counter]     = big_toy[current_index];
                    counter                = counter + 1;
                }
                std::cout<<"We had big toys left on elf. Revise the separation factor."<<std::endl;
                std::cout<<length<<" "<<counter<<std::endl;
            }
        }//while counter
    return DataFrame::create(_["Arrival"]= v_arrival,_["Duration"]  = v_duration_in,_["BigToy"]  = v_big_toy);
    
}//DataFrame

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
