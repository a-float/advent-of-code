import * as fs from 'fs'

const fields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid', 'cid'] as const;
const eyeColors = ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'];
const skipable = ['cid']
type Passport = { [key in typeof fields[number]]?: string }

const re = /(.+?):(.+?)\s/gm
const parsePassport = (passportString: string): Passport => {
    const pass: Passport = {};
    for (const match of passportString.matchAll(re)) {
        if (fields.includes(match[1] as any)) {
            pass[match[1] as typeof fields[number]] = match[2];
        } else {
            console.warn(`Unknown key-value pair: "${match[1]}:${match[2]}"`);
        }
    }
    return pass;
}

const isValid = (passport: Passport): boolean => {
    for (const field of fields) {
        if (passport[field] === undefined && !skipable.includes(field)) {
            return false;
        }
    }
    return true;
}

const isValidStrict = (passport: Passport): boolean => {
    if (!isValid(passport)) return false;
    const fullPassport = passport as Required<Passport> // validated by isValid

    if (!fullPassport.byr.match(/^\d{4}$/)) return false;
    const byr = parseInt(fullPassport.byr);
    if (byr < 1920 || byr > 2002) return false;

    if (!fullPassport.iyr.match(/^\d{4}$/)) return false;
    const iyr = parseInt(fullPassport.iyr);
    if (iyr < 2010 || iyr > 2020) return false;

    if (!fullPassport.eyr.match(/^\d{4}$/)) return false;
    const eyr = parseInt(fullPassport.eyr);
    if (eyr < 2020 || eyr > 2030) return false;

    if (!fullPassport.hgt.match(/^\d+(in|cm)$/)) return false;
    const hgt = parseInt(fullPassport.hgt);
    if (fullPassport.hgt.endsWith('in') && (hgt < 59 || hgt > 76)) return false;
    else if (fullPassport.hgt.endsWith('cm') && (hgt < 150 || hgt > 193)) return false;

    if (!fullPassport.hcl.match(/^#[a-f0-9]{6}$/)) return false;

    if (!eyeColors.includes(fullPassport.ecl)) return false;

    if (!fullPassport.pid.match(/^\d{9}$/)) return false;
    return true;
}

const filename = process.argv.length < 3 ? 'data.txt' : process.argv[2];
fs.readFile(filename, 'ascii', (err, content) => {
    if (err) {
        console.error("Unable to open file " + filename);
        process.exit(1);
    }
    const passports = content.split("\r\n\r\n")
        .map(passString => parsePassport(passString + ' ')) // add a space for easier regex parsing

    const validCount = passports
        .reduce((acc, passport) => +isValid(passport) + acc, 0);

    const validCountStrict = passports
        .reduce((acc, passport) => +isValidStrict(passport) + acc, 0)

    console.log("Number of all passports is " + passports.length);
    console.log("Number of valid passports is " + validCount);
    console.log("Number of strictly valid passports is " + validCountStrict);
});
