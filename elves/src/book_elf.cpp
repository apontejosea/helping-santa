#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double computeN(double t) {
    if (!(t >= 0))
        exit(1);
    if(0  <= t & t < 9)
        return 0;
    else if(9  <= t & t < 19)
        return(t-9);
    else
        return 10;
}

// [[Rcpp::export]]
double computeM(double t) {
    if (!(t >= 0)) exit(1);
    if(0  <= t & t < 9)
        return t;
    else if(9  <= t & t < 19) //0
        return 9;
    else
        return t-10;
}

//does this work like this?
// [[Rcpp::export]]
double diff_days(Datetime start, Datetime end) {
    Datetime newEnd, newStart;
    double theDays, origEndTime, origStartTime;

    theDays = 0;
    //remove time portion
    origEndTime = end.getHours()*60 + end.getMinutes();
    newEnd = end - origEndTime*60;
    origStartTime = start.getHours()*60 + start.getMinutes();
    newStart = start - origStartTime*60;
    theDays = (newEnd-newStart)/60.0/60.0/24.0;
    return theDays;
}

// [[Rcpp::export]]
double calc_sanctioned_hours(Datetime start, double duration) {
    Datetime expectedEnd;
    double expectedEndHr,startHr,sanctionedHours;
    
    expectedEnd = start + duration*60;
    expectedEndHr = expectedEnd.getHours() + expectedEnd.getMinutes()/60.0;
    startHr = start.getHours() + start.getMinutes()/60.0;
    sanctionedHours = computeN(expectedEndHr) - computeN(startHr) + diff_days(start, expectedEnd)*10; //revise diff_days()
    return sanctionedHours;
}

// [[Rcpp::export]]
double calc_unsanctioned_hours(Datetime start, double duration) {
    Datetime expectedEnd;
    double expectedEndHr,startHr, unsanctionedHours;
    
    expectedEnd = start + duration*60;
    expectedEndHr = expectedEnd.getHours() + expectedEnd.getMinutes()/60.0;
    startHr = start.getHours() + start.getMinutes()/60.0;
    unsanctionedHours = computeM(expectedEndHr) - computeM(startHr) + diff_days(start, expectedEnd)*14; //revise diff_days()
    return unsanctionedHours;
}

// [[Rcpp::export]]
double calc_p(double p0,double n,double m) {
    double p;
    
    p = std::min(4.0,std::max(0.25, p0*(pow(1.02,n)*pow(0.9,m))));
    return (p);
}

// [[Rcpp::export]]
Datetime next_9am(Datetime dtime) {
    Datetime the9AMDateTime;
    Datetime result;
    double origTime;
    int addition;
    
    origTime = dtime.getHours()*60 + dtime.getMinutes();
    the9AMDateTime  = dtime - origTime*60;
    addition = 9*60*60;
    if(dtime.getHours() > 9) {
        addition = addition + 24*60*60;
    }
    result = the9AMDateTime + addition;
    return result;
}


// [[Rcpp::export]]
Datetime pickStart(Datetime previousEnd, double previousM, int duration, double productivity, double threshold){
    Datetime s_e, s_a,the19HrDateTime;
    double origTime, restTime, n_a, m_a, p_a, n_e, m_e, p_e, advantage_index;
    
    origTime = previousEnd.getHours()*60 + previousEnd.getMinutes();
    the19HrDateTime  = previousEnd - origTime*60 + 19*60*60;
    
    if (the19HrDateTime >= previousEnd)
        restTime = 0;
    else{
    restTime = ((previousEnd.getHours()*60 + previousEnd.getMinutes()) - (the19HrDateTime.getHours()*60
                                                                          + the19HrDateTime.getMinutes()))*60;
    }
    //std::cout << "restTime: " <<restTime <<std::endl;

    s_e = previousEnd + restTime;
    s_a = next_9am(s_e) + restTime;
    n_a = calc_sanctioned_hours(s_a, duration);
    m_a = calc_unsanctioned_hours(s_a, duration);
    p_a = calc_p(productivity, n_a, m_a);
    n_e = calc_sanctioned_hours(s_e, duration);
    m_e = calc_unsanctioned_hours(s_e, duration);
    p_e = calc_p(productivity, n_e, m_e);
    
  /* std::cout << "n_a: " << n_a <<std::endl;
     std::cout << "m_a: " << m_a <<std::endl;
     std::cout << "p_a: " << p_a <<std::endl;
     std::cout << "n_e: " << n_e <<std::endl;
     std::cout << "m_e: " << m_e <<std::endl;
     std::cout << "p_e: " << p_e <<std::endl; */
    
    advantage_index = (p_a - p_e)*((s_a - s_e)/60.0/60.0);
    //std::cout << "advantage index: " << advantage_index <<std::endl;
    if(advantage_index > threshold)
        return(s_a);
    else
        return(s_e);
    
    return s_a;
}

// [[Rcpp::export]]
DataFrame book_elf_c(DatetimeVector& theArrival,NumericVector& theDuration, double threshold) {
            //variable declarations
            int theLength = theArrival.size();
            
            //cpp vectors that will eventually become a data frame
            //vectorType vectorName(size)
            NumericVector  thePVector(theLength);
            DatetimeVector theStartVector(theLength);
            DatetimeVector theEndVector(theLength);
            NumericVector  theNVector(theLength);
            NumericVector  theMVector(theLength);
            
            DatetimeVector the9AMVector(theLength);
            Datetime earliestStart(theLength);
            
            int origTime = 0;
            
            //initialize first entry of vectors
            thePVector[0] = 1;
            //get arrival timestamp and change hour to 9 AM
            origTime = theArrival[0].getHours()*60 +theArrival[0].getMinutes();
            the9AMVector[0]  = theArrival[0] - origTime*60 + 9*60*60;
            earliestStart = std::max(the9AMVector[0],theArrival[0]);
            theStartVector[0] = pickStart(earliestStart, 0, theDuration[0], thePVector[0], threshold);
            theEndVector[0] = theStartVector[0] + theDuration[0]/thePVector[0]*60;
            theNVector[0] = calc_sanctioned_hours(theStartVector[0], theDuration[0]);
            theMVector[0] = calc_unsanctioned_hours(theStartVector[0], theDuration[0]);
    
            for (int i=1; i<theLength; i++) {
                thePVector[i] = calc_p(thePVector[i-1],theNVector[i-1],theMVector[i-1]);
                earliestStart = pickStart(theEndVector[i-1], theMVector[i-1], theDuration[i], thePVector[i], threshold);
                theStartVector[i] = std::max(earliestStart,theArrival[i]);
                theEndVector[i] = theStartVector[i] + theDuration[i]/thePVector[i]*60;
                theNVector[i] = calc_sanctioned_hours(theStartVector[i], theDuration[i]);
                theMVector[i] = calc_unsanctioned_hours(theStartVector[i], theDuration[i]);
            }
    
            return DataFrame::create(_["p"]= thePVector, _["start"]= theStartVector,_["end"]= theEndVector,_["n"]= theNVector,_["m"]= theMVector);
}

