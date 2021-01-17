'''
BookKeeping Tool
Copyright (C) 2020, Javier Almeida

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.
'''

import math, datetime, argparse
import texttable


_INVEST_   = 'INVEST'
_RETURN_   = 'RETURN'
_UNITS_    = 'UNITS'
_ROI_      = 'ROI'
_AVG_      = 'AVG'
_DATE_     = 'DATE'
_CURRENCY_ = 'CURR'
_NAME_     = 'NAME'
_PRICE_    = 'PRICE'
_COMISSION_= 'COMISSION'
_DEFAULT_DATE_FORMAT_='%Y-%m-%d'

_NOELEMENT_=''

class Ledger():

    @staticmethod
    def VALIDATE_DATE(strval):
        DEFAULT_DATE_FORMAT='%Y-%m-%d'
        try:
            datetime.datetime.strptime(strval, DEFAULT_DATE_FORMAT)
            return(True)
        except:
            raise ValueError("Incorrect data format {}, should be {}".format(strval, DEFAULT_DATE_FORMAT))

    @staticmethod
    def VALIDATE_FLOAT(strval):
        try:
            _f=float(strval)
            return(True)
        except:
            raise ValueError('Incorrect float value: {}'.format(strval))

    @staticmethod
    def VALIDATE_POSITIVE_FLOAT(strval):
        try:
            f=float(strval)
        except:
            raise ValueError('Incorrect float value: {}'.format(strval))

        if f<0: raise ValueError('Positive float number required, negative value found: {}'.format(f))
        return(True)

    DESCRIPTOR= {_DATE_:     {'FIELDNR': 1, 'REQUIRED': True,   'TYPE':str,    'VALFUNC': VALIDATE_DATE           },
                 _NAME_:     {'FIELDNR': 2, 'REQUIRED': True,   'TYPE':str,    'VALFUNC': None                    },
                 _UNITS_:    {'FIELDNR': 3, 'REQUIRED': True,   'TYPE':float,  'VALFUNC': VALIDATE_FLOAT          },
                 _PRICE_:    {'FIELDNR': 4, 'REQUIRED': True,   'TYPE':float,  'VALFUNC': VALIDATE_POSITIVE_FLOAT },
                 _COMISSION_:{'FIELDNR': 5, 'REQUIRED': False,  'TYPE':float,  'VALFUNC': VALIDATE_POSITIVE_FLOAT  , 'DEFAULT':  0.0 },
                 _CURRENCY_: {'FIELDNR': 6, 'REQUIRED': False , 'TYPE':str,    'VALFUNC': None                     , 'DEFAULT': 'EUR'}}

    @classmethod
    def number_of_required_fields(cls):
        reqfields=[ key for key in cls.DESCRIPTOR if cls.DESCRIPTOR.get(key).get('REQUIRED') == True]
        return len(reqfields)

    @classmethod
    def skip_line(cls, ln):
        # Remove comments from line. This can be the whole line.
        nr_req_args=cls.number_of_required_fields()
        ln_wo_comment=ln.split('#')[0]
        ln_split=ln_wo_comment.split()
        nr_args=len(ln_split)
        if nr_args == 0:
            # Skip line
            return(True)
        elif nr_args >= nr_req_args:
            # Do not skip line
            return(False)
        else:
            message=  "Fewer fields than required found during line parsing. "
            message+= "Line content is '{}', ".format(ln_wo_comment)
            message+= "number of required arguments is {:d}".format(nr_req_args)
            raise ValueError(message)


    @classmethod
    def parse_line(cls, ln):
        # Remove comments from line. This can be the whole line.
        ln=ln.split('#')[0]
        ln_split=ln.split()
        ln_dict=dict()
        for key in cls.DESCRIPTOR.keys():
            nfield=cls.DESCRIPTOR.get(key).get('FIELDNR')
            valfunc=cls.DESCRIPTOR.get(key).get('VALFUNC')
            required_field=cls.DESCRIPTOR.get(key).get('REQUIRED')
            value=None
            # Read the value from corresponding field
            if nfield > len(ln_split):
                if required_field:
                    raise ValueError('Mandatory argument not present. Required {} in field {}'. format(key, nfield))
                else: value=cls.DESCRIPTOR.get(key).get('DEFAULT')
            else:
                value=ln_split[nfield-1]
            # Validate that the value of the field is legal
            if valfunc!=None:
                valfunc.__func__(value)
            # Perform type cast conversion according to descriptor type
            ln_dict[key]=cls.DESCRIPTOR.get(key).get('TYPE')(value)
        return(ln_dict)

    def __init__(self):
        self.ledger_lst=list()

    def parse(self, filename):
        FILE=open(filename,'r')
        for ln in FILE:
            if( Ledger.skip_line(ln) ): continue
            dct = Ledger.parse_line(ln)
            self.ledger_lst.append(dct)
        FILE.close()
        self.sort_by_date()
        return self

    def sort_by_date(self):
        def date2timestamp(dct):
            ts=datetime.datetime.strptime(dct[_DATE_], _DEFAULT_DATE_FORMAT_).timestamp()
            return ts

        self.ledger_lst=sorted(self.ledger_lst, key=date2timestamp, reverse=False)
        return self


    def dump(self, *args):
        if len(args)<1:
            return self.ledger_lst
        else:
            out=list()
            for key, val in args:
                out+=[ln for ln in self.ledger_lst if ln.get(key)==val]

        return(out)



class Portfolio_dict(dict):

    DEFAULT_PORTFOLIO_VALUE={_INVEST_:    0.0,
                             _RETURN_:    0.0,
                             _UNITS_:     0.0,
                             _ROI_:       0.0,
                             _AVG_:       0.0,
                             _DATE_:     '1999-12-31',
                             _CURRENCY_: 'EUR'}
    @staticmethod
    def create_key(symbol, currency):
        key=(symbol, currency)
        return(key)

    @staticmethod
    def remove_duplicates(lst_in):
        #Example: from [1, 2, 3, 1, 4, 9, 5, 5, 6, 7, 2, 9, 9, 3] returns [3, 9, 2, 7, 6, 5, 4, 1]
        n=len(lst_in)
        tmp=[k for k in lst_in]
        tmp.reverse()
        lst_out=[]
        for i in range(0, n):
            elem=tmp[i]
            if elem not in lst_out:
                lst_out.append(elem)
        return lst_out


    def update_symbol(self, dct):
        date   = dct[  _DATE_    ]
        ticker = dct[  _NAME_    ]
        units  = dct[  _UNITS_   ]
        price  = dct[  _PRICE_   ]
        com    = dct[_COMISSION_ ]
        curr   = dct[_CURRENCY_  ]

        # Create stock entry if not existing
        entry_key = self.create_key(ticker, curr)
        if entry_key not in self.keys():
            self.create_default_entry(entry_key)

        ticker_dct = self[entry_key]
        dct_old    = dict(ticker_dct)
        dct_new    = dict(ticker_dct)

        # Update portfolio numbers according to ledger entry
        dct_new[  _DATE_   ] = date
        dct_new[_CURRENCY_ ] = curr
        dct_new[  _UNITS_  ] = dct_old[_UNITS_] + units
        if dct_new[_UNITS_ ] < 0:
            raise Exception("negative number of units after processing '{}': previous value was {:7.4f}, new value is {:7.4f}".format(ticker,
                    float(dct_old[_UNITS_]),
                    float(dct_new[_UNITS_])))
        if units > 0:
            dct_new[ _INVEST_] = dct_old[_INVEST_] + units * price + com
            dct_new[  _AVG_  ] = (dct_old[_AVG_] * dct_old[_UNITS_] + units * price) / (dct_new[_UNITS_])
        if units < 0:
            dct_new[_INVEST_ ] = dct_old[_INVEST_] + com
            dct_new[_RETURN_ ] = dct_old[_RETURN_] + units * price
        if dct_new[ _UNITS_  ] > 0:
            dct_new[  _ROI_  ] = math.fabs(dct_new[_INVEST_] - math.fabs(dct_new[_RETURN_])) / dct_new[_UNITS_]
        else:
            dct_new[  _ROI_  ] = 0.0
        ticker_dct.update(dct_new)
        return dct_new

    def __init__(self, lst=None):
        dict.__init__(self)
        self.available_currencies=list()
        self.volume_history=list()
        self.gain_history=list()

        if lst!=None:
            for dct in lst:
                # Update history attribute
                updated_dct = self.update_symbol(dct)
                self.update_history( updated_dct[_DATE_], updated_dct[_CURRENCY_], self.compute_gain,   self.gain_history   )
                self.update_history( updated_dct[_DATE_], updated_dct[_CURRENCY_], self.compute_volume, self.volume_history )

        return None

    def compute_volume(self, currency):
        vol=sum( [ self[key][_UNITS_]*self[key][_AVG_] for key in self.keys() if self[key][_CURRENCY_]==currency ] )
        return vol

    def compute_gain(self, currency):
        def vol_f(key): return(self[key][_UNITS_] * self[key][_AVG_])
        def tba_f(key): return(abs(self[key][_RETURN_])-self[key][_INVEST_])
        vol=sum( [ tba_f(key) + vol_f(key) for key in self.keys() if self[key][_CURRENCY_]==currency] )
        return vol


    def update_history(self, date, currency, fn_handle, lst_out, ):
        def currency_to_index(self, currency):
            c2i=dict(zip(self.available_currencies, range(len(self.available_currencies))))
            try:
                indx=c2i.get(currency)
            except:
                raise ValueError("Requested currency '{:s} not found in object attribute.".format(currency))
            return indx

        def update_available_currencies(self, currency):
            if currency not in self.available_currencies:
                self.available_currencies.append(currency)
            return self

        update_available_currencies(self, currency )
        indx=currency_to_index(self, currency )
        vol=fn_handle(currency )
        lst=[_NOELEMENT_]*(len(self.available_currencies)+1)
        lst[0]=date
        lst[indx+1]=vol
        lst_out.append(lst)
        return self

    def return_history_attributes(self):
        #ToDo: organize'historizable' items inside a dict and generalize this routine to more than two items
        return self.available_currencies, self.volume_history, self.gain_history


    def create_default_entry(self, key):
        # Notice: the key is a non mutable tuple but the value is a mutable dictionary
        # This is the reason that the value has to be duplicated before updating
        #dict.update(self, {key: dict(Portfolio_dict.DEFAULT_PORTFOLIO_VALUE)} )
        self.update( {key: dict(Portfolio_dict.DEFAULT_PORTFOLIO_VALUE)} )
        return(self)

    def enum_keys_by_date(self):
        #returns a vector of the form [ [(key), date], ...,  ]
        tmp=[(tck, datetime.datetime.strptime(self.get(tck).get(_DATE_), _DEFAULT_DATE_FORMAT_).timestamp()) for tck in self.keys()]
        tmp=sorted(tmp, key=lambda it:it[1], reverse=False)
        return(tmp)

    def return_symbols_by_date(self):
        # pre-requisite: keys of the dictionary are a tuple of the form (symbol, currency)
        tmp=self.enum_keys_by_date()
        tmp=[key[0] for key, _date in tmp]
        tmp=Portfolio_dict.remove_duplicates(tmp)
        tmp.reverse()
        return tmp

    def return_keys_by_date(self):
        # pre-requisite: keys of the dictionary are a tuple of the form (symbol, currency)
        tmp=self.enum_keys_by_date()
        tmp=[key for key, _date in tmp]
        return tmp

    def return_all_currencies(self):
        cset=set()
        for key in self.keys():
            currency=self.get(key).get(_CURRENCY_)
            cset.add(currency)
        clst=sorted(list(cset), reverse=False)
        return(clst)

    def dump_by_key(self, key):
        out=Portfolio_dict()
        dct={key: dict(self.get(key)) }
        out.update(dct)
        return(out)


class Descriptor_Summary():
    @staticmethod
    def dat_f(dct, key): return(dct[_DATE_])
    @staticmethod
    def tic_f(dct, key): return(key[0])
    @staticmethod
    def uni_f(dct, key): return(dct[_UNITS_])
    @staticmethod
    def inv_f(dct, key): return(dct[_INVEST_])
    @staticmethod
    def ret_f(dct, key): return(dct[_RETURN_])
    @staticmethod
    def roi_f(dct, key): return(dct[_ROI_])
    @staticmethod
    def avg_f(dct, key): return(dct[_AVG_])
    @staticmethod
    def cur_f(dct, key): return(dct[_CURRENCY_])
    @staticmethod
    def vol_f(dct, key): return(dct[_UNITS_] * dct[_AVG_])
    @staticmethod
    def tba_f(dct, key): return(abs(dct[_RETURN_])-dct[_INVEST_])
    @staticmethod
    def gai_f(dct, key):
        vol=Descriptor_Summary.vol_f(dct, key)
        tba=Descriptor_Summary.tba_f(dct, key)
        return( vol + tba )
    @staticmethod
    def sum_f(col, table): return( sum( [ln[col] for ln in table if ln[col]!=None and ln[col]!='' ]) )

    NFIELDS=11
    PRECISION=4
    FSORT=Portfolio_dict.return_keys_by_date
    HEADER=['DATE'    , 'TICKER', 'UNITS',  'ROI' , 'AVG' , 'VOL' , 'TBAL' , 'CURR' ,  'GAIN' ]
    DTYPE =['t'       , 't'     , 'f'    ,  'f'   , 'f'   , 'f'   , 'f'    , 't'    ,  'f'    ]
    ALIGN =['c'       , 'l'     , 'r'    ,  'r'   , 'r'   , 'r'   , 'r'    , 'l'    ,  'r'    ]
    WIDTH =[ 12       ,  10     ,  8     ,   10   ,  10   ,  10   ,  10    ,  5     ,    10   ]
    BODY_F=[ dat_f    , tic_f   ,  uni_f ,  roi_f , avg_f , vol_f ,  tba_f ,  cur_f ,  gai_f  ]
    FOOT_F=[ 'TOTAL'  , None    , None   ,  None  , None  , sum_f ,  sum_f ,  None  ,  sum_f  ]



class TableType():
    ZERO_THRESHOLD=100
    def __init__(self, descriptor):
        self.TABLE=[]
        self.DESCRIPTOR = descriptor
        self.TABLE.append(self.DESCRIPTOR.HEADER)
        self.DECO=texttable.Texttable.BORDER|texttable.Texttable.VLINES

    def add_rows(self, portfolio):
        #ToDo: fix the line below...
        dict_keys= Descriptor_Summary.FSORT(portfolio)
        for key in dict_keys:
            tmp=portfolio[key]
            row=list()
            for fn in self.DESCRIPTOR.BODY_F:
                if fn==None or fn.__func__(tmp, key)==None: row.append('')
                else: row.append( fn.__func__(tmp, key) )
            self.TABLE.append(row)
        return(self)

    def load_table(self, table):
        self.TABLE=list(table)
        return self

    def add_footer(self):
        def is_callable(fn):
            if getattr(fn, '__func__', None)!=None:
                return True
            else:
                return False
        footer=list()
        nl=len(self.TABLE)
        table_wo_header=self.TABLE[1:nl]
        for col, fn in enumerate(self.DESCRIPTOR.FOOT_F):
            if fn == None: footer.append('')
            elif is_callable(fn) : footer.append( fn.__func__(col, table_wo_header) )
            else                 : footer.append( fn )
        self.TABLE.append(footer)
        return(self)

    def pretty_print(self):
        table = texttable.Texttable()
        table.set_deco(self.DECO)
        table.set_cols_dtype(self.DESCRIPTOR.DTYPE)
        table.set_cols_align(self.DESCRIPTOR.ALIGN)
        table.set_cols_width(self.DESCRIPTOR.WIDTH)
        table.add_rows(self.TABLE)
        print(table.draw())


class TableType1(TableType):
    def __init__(self, descriptor):
        TableType.__init__(self, descriptor)
        self.DECO=texttable.Texttable.BORDER|texttable.Texttable.VLINES

class TableType2(TableType):
    def __init__(self, descriptor):
        TableType.__init__(self, descriptor)
        self.DECO=texttable.Texttable.BORDER|texttable.Texttable.HEADER|texttable.Texttable.VLINES

class TableType3(TableType):
    class Descriptor_Volume_History():
        def __init__(self, n):
            self.DTYPE =['t'] + ['f']*(n-1)
            self.ALIGN =['c'] + ['r']*(n-1)
            self.WIDTH =[ 12] + [10 ]*(n-1)

    def __init__(self, data):
        self.TABLE=[]
        self.DESCRIPTOR=TableType3.Descriptor_Volume_History(len(data[0]))
        self.DECO=texttable.Texttable.BORDER|texttable.Texttable.HEADER|texttable.Texttable.VLINES
        self.add_rows(data)


    def add_rows(self, table):
        tmp=list(table)
        max_size=max([len(ln) for ln in tmp])
        for ln in tmp:
            if len(ln)<max_size: ln+= ['']*(max_size-len(ln))
        self.TABLE+=tmp
        return(self)


class TableType4(TableType):
    class Descriptor_ledger_with_history():
        def __init__(self, currency_lst):
            n=len(currency_lst)
            HEADER_vol =['VOLUME\n{:s}'.format(curr) for curr in currency_lst]
            HEADER_gai= ['GAIN\n{:s}'.format(curr) for curr in currency_lst]
            self.HEADER_led= [_DATE_, _NAME_, _UNITS_, _PRICE_, _COMISSION_, _CURRENCY_]
            self.HEADER=     [_DATE_, _NAME_, _UNITS_, _PRICE_, _COMISSION_, _CURRENCY_] + HEADER_vol + HEADER_gai
            self.DTYPE =     ['t'   , 't'   , 'f'    , 'f'    , 'f'        , 't'       ] + ['f']*n    +  ['f']*n
            self.ALIGN =     ['c'   , 'l'   , 'r'    , 'r'    , 'r'        , 'l'       ] + ['r']*n    +  ['r']*n
            self.WIDTH =     [ 12   ,  10   ,  8     ,  10    ,  9         ,  5        ] + [ 10]*n    +  [ 10]*n


    def __init__(self, currency_lst, ledger_dct, vol_lst, gain_lst):
        def validate_date_consistency(ledger_dct, vol_lst, gain_lst):
            # Check that the dates of the three tables are the same
            ln     = len(ledger_dct)
            ln_vol = len(vol_lst)
            ln_gain= len(gain_lst)
            if ln != ln_vol or ln != ln_gain:
                raise ValueError('Inconsistent data structures trying to combine tables. Size don´t match {:d}!={:d}!={:d}'.format(ln, ln_vol, ln_gain))
            for k in range(0, ln):
                date=ledger_dct[k].get(_DATE_)
                date1=vol_lst[k][0]
                date2=gain_lst[k][0]
                if date!=date1 or date!=date2:
                    raise ValueError('Non-matching dates trying to combine tables: {:s}!={:s}!={:s}'.format(date, date1, date2))
            return None

        self.TABLE=[]
        self.DESCRIPTOR=TableType4.Descriptor_ledger_with_history(currency_lst)
        self.DECO=texttable.Texttable.BORDER|texttable.Texttable.HEADER|texttable.Texttable.VLINES
        validate_date_consistency(ledger_dct, vol_lst, gain_lst)

        ncols=len(currency_lst)
        table_led=[ [ dct.get(key) for key in self.DESCRIPTOR.HEADER_led] for dct in ledger_dct]
        table_vol=[ ln[1:len(ln)] + [_NOELEMENT_]*(ncols-len(ln)+1) for ln in vol_lst]
        table_gai=[ ln[1:len(ln)] + [_NOELEMENT_]*(ncols-len(ln)+1) for ln in gain_lst]
        self.TABLE.append(self.DESCRIPTOR.HEADER)
        self.TABLE+= [table_led[k]+table_vol[k]+table_gai[k] for k in range(0, len(table_led))]



def display_portfolio_history(ledger):
    portfolio_full=Portfolio_dict(ledger.dump())
    symbol_lst=portfolio_full.return_symbols_by_date()
    for symbol in symbol_lst:
        subledger=ledger.dump( ( _NAME_ , symbol) )
        if len(subledger) < 1: raise Exception("Entries for the provided symbol '{}' not found in ledger".format(symbol))
        table=TableType1(Descriptor_Summary() )
        for k in range(0, len(subledger)):
            currency=subledger[k][ _CURRENCY_ ]
            key=Portfolio_dict.create_key(symbol, currency)
            portfolio=Portfolio_dict(subledger[0:k+1])
            subportfolio=portfolio.dump_by_key(key)
            table.add_rows(subportfolio)
        table.pretty_print()




if __name__== "__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument('filename', help='file name of ascii ledger with full path', type=str)
    args=parser.parse_args()
    filename=args.filename

    # Parse ledger file
    ledger=Ledger()
    ledger.parse(filename)
    
    # Table I: display ledger plus volume/gain
    portfolio=Portfolio_dict(ledger.ledger_lst)
    currency_lst, vol_table, gain_table=portfolio.return_history_attributes()
    t4=TableType4( currency_lst, ledger.dump(), vol_table, gain_table )
    t4.pretty_print()
    
    
    # Table II: display history per symbol
    display_portfolio_history(ledger)
    
    # Table III: display portfolio status to date    
    for currency in currency_lst:
        portfolio=Portfolio_dict(ledger.dump( ( _CURRENCY_ , currency) ) )
        ptable =TableType2(Descriptor_Summary() )
        ptable.add_rows(portfolio).add_footer().pretty_print()
    







