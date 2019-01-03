contract Identity =

  record date_time =
    {
      year : int,
      month : int,
      day : int,
      hour : int,
      minute : int,
      second : int,
      weekday : int
    }

  record state =
  	{
      day_in_seconds : int,
      year_in_seconds : int,
      leap_year_in_seconds : int,
      hour_in_seconds : int,
      minute_in_seconds : int,
      origin_year : int
    }

  public function init() : state =
    { day_in_seconds = 86400,
      year_in_seconds = 31536000,
      leap_year_in_seconds = 31622400,
      hour_in_seconds = 3600,
      minute_in_seconds = 60,
      origin_year = 1970 }

  public function is_leap_year(year : int) : bool =
    if(modulo(year, 4) != 0)
    	false
    elif(modulo(year, 100) != 0)
    	true
    elif(modulo(year, 400) != 0)
    	false
    else
    	true

  public function leap_years_before(year : int) : int =
    let year_sub = year - 1
    year_sub / 4 - year_sub / 100 + year_sub / 400

  public function get_days_in_month(month : int, year : int) : int =
    if(month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12)
      31
    elif(month == 4 || month == 6 || month == 9 || month == 11)
      30
    elif(is_leap_year(year))
      29
    else
      28

  public function parse_timestamp(timestamp : int) : int =
    let dt : date_time = date_time_init()

    // year
    let dt = dt{ year = get_year(timestamp) }
    let buf : int = leap_years_before(dt.year) - leap_years_before(state.origin_year)

    let seconds_accounted_for = state.leap_year_in_seconds * buf +
      state.year_in_seconds * (dt.year - state.origin_year - buf)

    // month

    1

  public function get_year(timestamp : int) : int =
    let year = state.origin_year + timestamp / state.year_in_seconds
    let num_leap_years = leap_years_before(year) - leap_years_before(state.origin_year)
    let seconds_accounted_for = state.leap_year_in_seconds * num_leap_years +
      state.year_in_seconds * (year - state.origin_year - num_leap_years)

    _get_year(timestamp, seconds_accounted_for, year)

  private function _get_year(timestamp : int, seconds_accounted_for : int, year : int) : int =
    if(seconds_accounted_for =< timestamp)
      year
    else
      let year = year - 1
      let seconds_accounted_for =
        if(is_leap_year(year - 1))
          seconds_accounted_for - state.leap_year_in_seconds
        else
          seconds_accounted_for - state.year_in_seconds

      _get_year(timestamp, seconds_accounted_for, year - 1)

  private function date_time_init() : date_time =
    { year = state.origin_year,
      month = 0,
      day = 0,
      hour = 0,
      minute = 0,
      second = 0,
      weekday = 0 }

  private function modulo(num : int, divisor : int) : int =
  	num - divisor * (num / divisor)
