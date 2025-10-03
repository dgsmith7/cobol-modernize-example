import React, { useState } from "react";

export default function AccountForm({ onSuccess }) {
  const [accountNumber, setAccountNumber] = useState("");
  const [customerName, setCustomerName] = useState("");
  const [initialBalance, setInitialBalance] = useState("");
  const [message, setMessage] = useState("");
  const [messageType, setMessageType] = useState("info"); // "success" or "danger"

  const validate = () => {
    if (!/^\d{10}$/.test(accountNumber)) {
      setMessage("Account number must be exactly 10 digits.");
      setMessageType("danger");
      return false;
    }
    if (!customerName.trim()) {
      setMessage("Customer name is required.");
      setMessageType("danger");
      return false;
    }
    if (isNaN(initialBalance) || Number(initialBalance) < 0) {
      setMessage("Initial balance must be a positive number.");
      setMessageType("danger");
      return false;
    }
    return true;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setMessage("");
    if (!validate()) return;
    const res = await fetch("http://localhost:3001/api/accounts", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        accountNumber,
        customerName,
        initialBalance: parseFloat(initialBalance),
      }),
    });
    const data = await res.json();
    if (data.success) {
      setMessage("Account created successfully!");
      setMessageType("success");
      setAccountNumber("");
      setCustomerName("");
      setInitialBalance("");
      if (onSuccess) onSuccess();
    } else {
      setMessage(data.error?.message || "Account creation failed.");
      setMessageType("danger");
    }
  };

  return (
    <form
      className="card p-3 mb-4"
      style={{ maxWidth: 500 }}
      onSubmit={handleSubmit}
    >
      <h3 className="mb-3">Create New Account</h3>
      <div className="mb-3">
        <label className="form-label">Account Number (10 digits)</label>
        <input
          className="form-control"
          type="text"
          placeholder="Account Number"
          value={accountNumber}
          onChange={(e) => setAccountNumber(e.target.value)}
          required
        />
      </div>
      <div className="mb-3">
        <label className="form-label">Customer Name</label>
        <input
          className="form-control"
          type="text"
          placeholder="Customer Name"
          value={customerName}
          onChange={(e) => setCustomerName(e.target.value)}
          required
        />
      </div>
      <div className="mb-3">
        <label className="form-label">Initial Balance</label>
        <input
          className="form-control"
          type="number"
          step="0.01"
          min="0"
          placeholder="Initial Balance"
          value={initialBalance}
          onChange={(e) => setInitialBalance(e.target.value)}
          required
        />
      </div>
      <button className="btn btn-primary" type="submit">
        Create Account
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
