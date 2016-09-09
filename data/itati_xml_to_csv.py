try:
    import xml.etree.cElementTree as ET
except ImportError:
    import xml.etree.ElementTree as ET

import sys

tree = ET.ElementTree(file='activity.xml')
n_ex = 0

for activity in tree.iterfind('//iati-activity'):
  try:
    iati_identifier = activity.find('iati-identifier').text.encode('utf-8').strip()
    reporting_org = activity.find('reporting-org')
    if not 'ref' in reporting_org.attrib:
      print "warning, no reporting org ref for " + iati_identifier
      continue
    reporting_org_ref = reporting_org.attrib['ref'].strip()

    default_currency = "EUR"
    if ('default-currency' in activity.attrib):
      default_currency = activity.attrib['default-currency'].strip().upper()
    
    participating_orgs_score = 0
    participating_orgs_refs = 0
    participating_orgs_activity_ids = 0
    participating_orgs_types = 0

    participating_orgs = activity.findall('participating-org')
    for participating_org in participating_orgs:
      if ('ref' in participating_org.attrib and participating_org.attrib['ref'].strip() != ""):
        participating_orgs_refs += 1
      if ('activity-id' in participating_org.attrib and participating_org.attrib['activity-id'].strip() != ""):
        participating_orgs_activity_ids += 1
      if ('type' in participating_org.attrib and participating_org.attrib['type'].strip() != ""):
        participating_orgs_types +=1

    if (len(participating_orgs) > 0):
      participating_orgs_score = (participating_orgs_refs+participating_orgs_activity_ids+participating_orgs_types)/(3.0*len(participating_orgs))

    transactions = activity.findall('transaction')
    transaction_score = 0

    total_cost = 0

    for transaction in transactions:
      provider_org = transaction.find('provider-org')
      if (provider_org != None and 'ref' in provider_org.attrib and provider_org.attrib['ref'].strip() != ""):
        transaction_score += 1

      receiver_org = transaction.find('receiver-org')
      if (receiver_org != None and 'ref' in receiver_org.attrib and receiver_org.attrib['ref'].strip() != ""):
        transaction_score += 1

      if transaction.find('flow-type') != None:
        transaction_score += 1

      if transaction.find('finance-type') != None:
        transaction_score += 1

      if transaction.find('aid-type') != None:
        transaction_score += 1

      if transaction.find('tied-status') != None:
        transaction_score += 1

      transaction_score += 1

      value = transaction.find('value')
      value_n = float(value.text.strip())
      value_currency = default_currency
      if ('currency' in value.attrib):
        value_currency = value.attrib['currency'].strip().upper()
      if (value_currency == "EUR" or value_currency == "EURO"):
        total_cost += value_n
      elif value_currency == "USD":
        total_cost += value_n * 0.89
      elif value_currency == "GBP":
        total_cost += value_n * 1.18
      elif value_currency == "PKR":
        total_cost += value_n * 0.0085
      elif value_currency == "NOK":
        total_cost += value_n * 0.11
      elif value_currency == "DKK":
        total_cost += value_n * 0.13
      elif value_currency == "AUD":
        total_cost += value_n * 0.67
      elif value_currency == "JPY":
        total_cost += value_n * 0.0086
      elif value_currency == "CAD":
        total_cost += value_n * 0.68
      elif value_currency == "CHF":
        total_cost += value_n * 0.91
      elif value_currency == "CHF":
        total_cost += value_n * 0.91
      elif value_currency == "AFN":
        total_cost += value_n * 0.013
      elif value_currency == "AFN":
        total_cost += value_n * 0.013
      elif value_currency == "AUD":
        total_cost += value_n * 0.67
      elif value_currency == "BDT":
        total_cost += value_n * 0.011
      elif value_currency == "BWP":
        total_cost += value_n * 0.083
      elif value_currency == "ZAR":
        total_cost += value_n * 0.062
      elif value_currency == "COP":
        total_cost += value_n * 0.00031
      elif value_currency == "LTL":
        total_cost += value_n * 0.29
      elif value_currency == "RON":
        total_cost += value_n * 0.22
      elif value_currency == "XDR":
        total_cost += value_n * 1.25
      elif value_currency == "":
         transaction_score -= 1
      else:
        print >>sys.stderr, "unknown currency " + value_currency
        raise

    if len(transactions) > 0:
      transaction_score = transaction_score/(6.0*len(transactions))

    print reporting_org_ref + "\t" + iati_identifier + "\t" + str(participating_orgs_score) +  "\t"  + str(transaction_score)   +  "\t"  + str(total_cost) + "\t"  + str(len(transactions)) 
  except:
    n_ex += 1

print >>sys.stderr, n_ex
