REGEXP_REPLACE(
  business_name,
  r'(?i)[\s.,-]*(ll[cp]|inc(orporated)?|ltd|co(rp(oration)?)?|company|group|holdings|ent(erprise)?|assoc(iates)?)[\s.,-]*',
  ' '
)