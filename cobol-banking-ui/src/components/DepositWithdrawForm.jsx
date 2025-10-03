import React, { useState } from "react";

export default function DepositWithdrawForm({ accountNumber, onSuccess }) {
  const [amount, setAmount] = useState("");
  const [type, setType] = useState("deposit");
  const [message, setMessage] = useState("");
  const [messageType, setMessageType] = useState("info"); // "success" or "danger"

  const validate = () => {
    if (isNaN(amount) || Number(amount) <= 0) {
      setMessage("Amount must be a positive number.");
      setMessageType("danger");
      return false;
    }
    return true;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setMessage("");
    if (!validate()) return;
    const endpoint =
      type === "deposit"
        ? `/api/accounts/${accountNumber}/deposit`
        : `/api/accounts/${accountNumber}/withdraw`;

    const res = await fetch(`http://localhost:3001${endpoint}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ amount: parseFloat(amount) }),
    });
    const data = await res.json();
    if (data.success) {
      setMessage("Transaction successful!");
      setMessageType("success");
      setAmount("");
      if (onSuccess) onSuccess();
    } else {
      setMessage(data.error?.message || "Transaction failed.");
      setMessageType("danger");
    }
  };

  return (
    <form
      className="card p-3 mb-4"
      style={{ maxWidth: 500 }}
      onSubmit={handleSubmit}
    >
      <h3 className="mb-3">Deposit / Withdraw</h3>
      <div className="mb-3">
        <label className="form-label">Transaction Type</label>
        <select
          className="form-select"
          value={type}
          onChange={(e) => setType(e.target.value)}
        >
          <option value="deposit">Deposit</option>
          <option value="withdraw">Withdraw</option>
        </select>
      </div>
      <div className="mb-3">
        <label className="form-label">Amount</label>
        <input
          className="form-control"
          type="number"
          step="0.01"
          min="0"
          placeholder="Amount"
          value={amount}
          onChange={(e) => setAmount(e.target.value)}
          required
        />
      </div>
      <button className="btn btn-success" type="submit">
        Submit
      </button>
      {message && (
        <div
          className={`mt-3 alert alert-${messageType} text-center fw-bold`}
          role="alert"
        >
          {message}
        </div>
      )}
    </form>
  );
}
