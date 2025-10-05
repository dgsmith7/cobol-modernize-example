const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

const rootDir = path.resolve(__dirname, ".."); // cobol-banking
const binPath = path.resolve(rootDir, "bin", "BANKLEDG");
const dataDir = path.resolve(__dirname);
const files = {
  ACCOUNTS: path.join(dataDir, "ACCOUNTS.DAT"),
  TRANSACT: path.join(dataDir, "TRANSACT.DAT"),
  COUNTER: path.join(dataDir, "COUNTER.DAT"),
};

function execCobol(args, opts = {}) {
  const res = spawnSync(binPath, args, {
    cwd: rootDir,
    encoding: "utf8",
    ...opts,
  });
  return {
    status: res.status,
    stdout: (res.stdout || "").trim(),
    stderr: (res.stderr || "").trim(),
  };
}

function ensureBin() {
  if (!fs.existsSync(binPath)) {
    console.error(`ERROR: COBOL binary not found at ${binPath}`);
    process.exit(1);
  }
  try {
    fs.chmodSync(binPath, 0o755);
  } catch (e) {}
}

function clearFiles() {
  Object.values(files).forEach((f) => {
    try {
      fs.writeFileSync(f, "");
      console.log(`Cleared: ${f}`);
    } catch (err) {
      console.warn(`Warning clearing ${f}: ${err.message}`);
    }
  });
}

function randInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function uniqueAccountNumbers(count) {
  const set = new Set();
  while (set.size < count) {
    const num = String(randInt(1_000_000_000, 9_999_999_999)); // 10 digits
    set.add(num);
  }
  return Array.from(set);
}

const N_ACCOUNTS = 15;
const MIN_TX_PER = 5;
const MAX_TX_PER = 10;

// First and last name lists to guarantee First_Last (underscore) for every account
const FIRST_NAMES = [
  "Alice",
  "Bob",
  "Carol",
  "David",
  "Evelyn",
  "Frank",
  "Grace",
  "Henry",
  "Isabel",
  "James",
  "Katherine",
  "Liam",
  "Maya",
  "Noah",
  "Olivia",
];
const LAST_NAMES = [
  "Johnson",
  "Martinez",
  "Nguyen",
  "Lee",
  "Smith",
  "Brown",
  "Kim",
  "Wilson",
  "Garcia",
  "Thompson",
  "Lopez",
  "Anderson",
  "Patel",
  "Walker",
  "Robinson",
];

function generateUniqueNames(count) {
  const names = new Set();
  while (names.size < count) {
    const first = FIRST_NAMES[randInt(0, FIRST_NAMES.length - 1)];
    const last = LAST_NAMES[randInt(0, LAST_NAMES.length - 1)];
    // Use underscore to keep the name a single token for COBOL CLI
    names.add(`${first}_${last}`);
  }
  return Array.from(names);
}

// main
(function main() {
  ensureBin();
  clearFiles();

  const fullNames = generateUniqueNames(N_ACCOUNTS);
  const acctNumbers = uniqueAccountNumbers(N_ACCOUNTS);
  const accounts = {}; // acctNumber -> { name, balance }

  // create accounts
  for (let i = 0; i < acctNumbers.length; i++) {
    const acct = acctNumbers[i];
    const name = fullNames[i];
    const initial = (randInt(1000, 10000) + Math.random()).toFixed(2);
    console.log(`CREATE ${acct} ${name} ${initial}`);
    const res = execCobol(["CREATE", acct, name, initial]);
    if (res.status !== 0) {
      console.warn(`CREATE failed for ${acct}: ${res.stderr || res.stdout}`);
      // continue anyway to allow dataset population
    }
    accounts[acct] = { name, balance: parseFloat(initial) };
  }

  // ensure representation of all transaction types across dataset
  const typeSeen = { D: 0, W: 0, T: 0 };

  // generate and execute transactions
  const acctList = Object.keys(accounts);
  for (const acct of acctList) {
    const nTx = randInt(MIN_TX_PER, MAX_TX_PER);
    for (let t = 0; t < nTx; t++) {
      const pick = Math.random();
      let type = pick < 0.45 ? "D" : pick < 0.85 ? "W" : "T";
      const amount = parseFloat((randInt(10, 2000) + Math.random()).toFixed(2));

      if (type === "D") {
        const res = execCobol(["DEPOSIT", acct, String(amount)]);
        if (res.status === 0) {
          accounts[acct].balance += amount;
          typeSeen.D++;
        } else {
          console.warn(
            `DEPOSIT failed ${acct} ${amount}: ${res.stderr || res.stdout}`
          );
        }
      } else if (type === "W") {
        const maxAllow = accounts[acct].balance + 1000;
        const amt = Math.min(
          amount,
          Math.max(1, parseFloat(maxAllow.toFixed(2)))
        );
        if (amt <= 0) {
          const res = execCobol(["DEPOSIT", acct, String(Math.abs(amount))]);
          if (res.status === 0) accounts[acct].balance += Math.abs(amount);
          continue;
        }
        const res = execCobol(["WITHDRAW", acct, String(amt)]);
        if (res.status === 0) {
          accounts[acct].balance -= amt;
          typeSeen.W++;
        } else {
          console.warn(
            `WITHDRAW failed ${acct} ${amt}: ${res.stderr || res.stdout}`
          );
        }
      } else if (type === "T") {
        const otherCandidates = acctList.filter((a) => a !== acct);
        if (otherCandidates.length === 0) continue;
        const other = otherCandidates[randInt(0, otherCandidates.length - 1)];
        const maxAllow = accounts[acct].balance + 1000;
        const amt = Math.min(
          amount,
          Math.max(1, parseFloat(maxAllow.toFixed(2)))
        );
        if (amt <= 0) {
          const res = execCobol(["DEPOSIT", other, String(amount)]);
          if (res.status === 0) accounts[other].balance += amount;
          continue;
        }

        let res = execCobol(["TRANSFER", acct, other, String(amt)]);
        if (
          res.status === 0 &&
          !/requires full implementation/i.test(res.stdout + res.stderr)
        ) {
          accounts[acct].balance -= amt;
          accounts[other].balance += amt;
          typeSeen.T++;
        } else {
          const r1 = execCobol(["WITHDRAW", acct, String(amt)]);
          const r2 = execCobol(["DEPOSIT", other, String(amt)]);
          if (r1.status === 0 && r2.status === 0) {
            accounts[acct].balance -= amt;
            accounts[other].balance += amt;
            typeSeen.T++;
          } else {
            console.warn(
              `Fallback transfer failed: ${acct}->${other} ${amt}`,
              r1.stderr || r1.stdout,
              r2.stderr || r2.stdout
            );
          }
        }
      }
    }
  }

  // ensure at least one of each transaction type exists
  if (typeSeen.D === 0 && acctList.length > 0) {
    const acct = acctList[0],
      amt = 100.0;
    const res = execCobol(["DEPOSIT", acct, String(amt)]);
    if (res.status === 0) accounts[acct].balance += amt;
    typeSeen.D++;
  }
  if (typeSeen.W === 0 && acctList.length > 1) {
    const acct = acctList[1],
      amt = 50.0;
    const res = execCobol(["WITHDRAW", acct, String(amt)]);
    if (res.status === 0) accounts[acct].balance -= amt;
    typeSeen.W++;
  }
  if (typeSeen.T === 0 && acctList.length > 2) {
    const a = acctList[2],
      b = acctList[3] || acctList[0],
      amt = 25.0;
    let res = execCobol(["TRANSFER", a, b, String(amt)]);
    if (
      res.status === 0 &&
      !/requires full implementation/i.test(res.stdout + res.stderr)
    ) {
      accounts[a].balance -= amt;
      accounts[b].balance += amt;
      typeSeen.T++;
    } else {
      const r1 = execCobol(["WITHDRAW", a, String(amt)]);
      const r2 = execCobol(["DEPOSIT", b, String(amt)]);
      if (r1.status === 0 && r2.status === 0) {
        accounts[a].balance -= amt;
        accounts[b].balance += amt;
        typeSeen.T++;
      }
    }
  }

  // write COUNTER.DAT with a simple counter value
  try {
    fs.writeFileSync(files.COUNTER, "1\n");
  } catch (err) {
    console.warn("Unable to write COUNTER.DAT:", err.message);
  }

  console.log("--- Summary ---");
  console.log(`Accounts created: ${acctList.length}`);
  console.log(`Transactions per account: ${MIN_TX_PER}-${MAX_TX_PER}`);
  console.log("Transaction types seen:", typeSeen);

  console.log("Sample accounts:");
  acctList.slice(0, 10).forEach((a) => {
    const acc = accounts[a];
    console.log(`${a}  ${acc.name}  Balance: ${acc.balance.toFixed(2)}`);
  });

  console.log("Done. Inspect data files in:", dataDir);
})();
