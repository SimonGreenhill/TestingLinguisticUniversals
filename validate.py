#!/usr/bin/env python3
# coding=utf-8
"""..."""
__author__ = 'Simon J. Greenhill <simon@simon.net.nz>'
__copyright__ = 'Copyright (c) 2025 Simon J. Greenhill'
__license__ = 'New-style BSD'

import sys
from pathlib import Path

RESULTS = Path('results/')

EXPECTED = [
    '0001_or_2aKA',
    '0001_or_2bKA',
    '0005KA',
    '0008KA',
    '0015aKA',
    '0015bKA',
    '0045KA',
    '0055KA',
    '0056KA',
    '0057aKA',
    '0057bKA',
    '0062aKA',
    '0062bKA',
    '0065_or_66KA',
    '0067_or_68KA',
    '0069KA',
    '0070KA',
    '0071KA',
    '0072KA',
    '0073KA',
    '0074KA',
    '0075KA',
    '0084KA',
    '0087KA',
    '0088KA',
    '0091KA',
    '0093KA',
    '0095aKA',
    '0095bKA',
    '0096KA',
    '0101KA',
    '0107aKA',
    '0107bKA',
    '0107cKA',
    '0108KA',
    '0110KA',
    '0116KA',
    '0121KA',
    '0125KA',
    '0126nKA',
    '0144KA',
    '0145KA',
    '0147KA',
    '0166KA',
    '0173KA',
    '0174KA',
    '0175KA',
    '0176KA',
    '0177KA',
    '0178KA',
    '0179aKA',
    '0179bKA',
    '0179cKA',
    '0183KA',
    '0228KA',
    '0229KA',
    '0230aKA',
    '0230bKA',
    '0230cKA',
    '0230dKA',
    '0230eKA',
    '0230fKA',
    '0231aKA',
    '0231bKA',
    '0232KA',
    '0237aKA',
    '0238aKA',
    '0238bKA',
    '0238dKA',
    '0239aKA',
    '0239bKA',
    '0239cKA',
    '0240aKA',
    '0245aKA',
    '0245bKA',
    '0247KA',
    '0260KA',
    '0276KA',
    '0320KA',
    '0328KA',
    '0331aKA',
    '0331bKA',
    '0332KA',
    '0344KA',
    '0356KA',
    '0357KA',
    '0365KA',
    '0386KA',
    '0423KA',
    '0439KA',
    '0446KA',
    '0454KA',
    '0489KA',
    '0493aKA',
    '0493bKA',
    '0504aKA',
    '0504bKA',
    '0506KA',
    '0507KA',
    '0511aKA',
    '0511bKA',
    '0513KA',
    '0516KA',
    '0517KA',
    '0521KA',
    '0564KA',
    '0569KA',
    '0572KA',
    '0573KA',
    '0577KA',
    '0582KA',
    '0612KA',
    '0668KA',
    '0798KA',
    '0822KA',
    '0837KA',
    '0889aKA',
    '0889bKA',
    '0917aKA',
    '0917bKA',
    '0917cKA',
    '0964aKA',
    '0964bKA',
    '0982KA',
    '0983KA',
    '1027aKA',
    '1027bKA',
    '1027cKA',
    '1028aKA',
    '1028bKA',
    '1028cKA',
    '1053KA',
    '1111KA',
    '1114KA',
    '1142aKA',
    '1142bKA',
    '1152KA',
    '1163KA',
    '1216KA',
    '1267aKA',
    '1267bKA',
    '1334aKA',
    '1334bKA',
    '1334cKA',
    '1341KA',
    '1349KA',
    '1372KA',
    '1415KA',
    '1427KA',
    '1430KA',
    '1431mKA',
    '1431nKA',
    '1434KA',
    '1436KA',
    '1499KA',
    '1531KA',
    '1534KA',
    '1537KA',
    '1544KA',
    '1545KA',
    '1546aKA',
    '1546bKA',
    '1548aKA',
    '1548bKA',
    '1549KA',
    '1550KA',
    '1559KA',
    '1560KA',
    '1561KA',
    '1586KA',
    '1589KA',
    '1591KA',
    '1592KA',
    '1593KA',
    '1610KA',
    '1611KA',
    '1612aKA',
    '1612bKA',
    '1612cKA',
    '1612dKA',
    '1621KA',
    '1653KA',
    '1667aKA',
    '1667bKA',
    '1741KA',
    '1827KA',
    '1994aKA',
    '1994bKA',
    '1994cKA',
    '1994dKA',
    '2012KA',
]


BRMS_SP_EXPECTED = [
    'summary.txt',
    'summary_clean.txt',
    'ranef_spatial.txt',
    'ranef_phylo.txt',
    'ranef_macroarea_V3.txt',
    'ranef_macroarea_intercept.txt',
]

BRMS_SP_POSTERIOR_EXPECTED = [
    'summary.%d.txt',
    'summary_clean.%d.txt',
    'ranef_spatial.%d.txt',
    'ranef_phylo.%d.txt',
    'ranef_macroarea_V3.%d.txt',
    'ranef_macroarea_intercept.%d.txt',
]


def has_file(f):
    assert f.exists(), 'Missing %s'


def has_file(f):
    if not f.exists():
        print("ERROR: Missing %s" % f)
        return False
    return True



if __name__ == '__main__':
    for analysis in EXPECTED:
        axdir = RESULTS / analysis
        assert axdir.is_dir()
        
        # should have a BT_data.txt
        has_file(axdir / 'BT_data.txt')

        # should have script_*
        gbfiles = [f for f in axdir.glob("*.R") if f.name.startswith("script_")]
        assert len(gbfiles)
        
        # should have pruned_tree.tree, and posterior pruned trees.
        assert has_file(axdir / 'pruned_tree.tree')
        assert has_file(axdir / 'pruned_tree.trees.gz')
        
        # check brms.single
        bs = axdir / 'brms.single'
        has_file(bs)
        assert bs.exists()
        for f in ['summary_clean_uncontrolled.txt', 'summary_uncontrolled.txt']:
            has_file(bs / f)
        
        bsp = axdir / 'brms_spphylo.single'
        for f in BRMS_SP_EXPECTED:
            has_file(bsp / f)

        # check brms.posterior
        bs = axdir / 'brms.posterior'
        has_file(bs)
        assert bs.exists()
        
        for expected in BRMS_SP_POSTERIOR_EXPECTED:
            for i in range(1, 101):
                has_file(bs / (expected % i))
