/**
 * FsActorCheckpoint - File-system based actor checkpoint implementing IActorCheckpoint
 *
 * WAL (Write-Ahead Log) + Snapshots pattern for durable state.
 * Uses node:fs/promises (works on both Node and Bun).
 */

import { writeFile, readFile, mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import type { IActorCheckpoint } from '@agentic-primer/actors';

export class FsActorCheckpoint implements IActorCheckpoint {
  private readonly dataDir: string;

  constructor(dataDir: string) {
    this.dataDir = dataDir;
  }

  async initialize(): Promise<void> {
    await mkdir(this.dataDir, { recursive: true });
  }

  async loadSnapshot(key: string): Promise<Uint8Array | null> {
    const snapshotPath = join(this.dataDir, `${key}.snapshot`);
    if (!existsSync(snapshotPath)) return null;

    const buffer = await readFile(snapshotPath);
    return new Uint8Array(buffer);
  }

  async saveSnapshot(key: string, data: Uint8Array): Promise<void> {
    const snapshotPath = join(this.dataDir, `${key}.snapshot`);
    // Atomic write: write to temp then rename
    const tempPath = `${snapshotPath}.tmp`;
    await writeFile(tempPath, data);
    const { rename } = await import('node:fs/promises');
    await rename(tempPath, snapshotPath);
  }

  async appendWAL(key: string, entry: Uint8Array): Promise<void> {
    const walPath = join(this.dataDir, `${key}.wal`);
    // Length-prefixed entries for reliable replay
    const length = new Uint32Array([entry.length]);
    const header = new Uint8Array(length.buffer);
    const combined = new Uint8Array(header.length + entry.length);
    combined.set(header);
    combined.set(entry, header.length);
    await writeFile(walPath, combined, { flag: 'a' });
  }

  async replayWAL(key: string): Promise<Uint8Array[]> {
    const walPath = join(this.dataDir, `${key}.wal`);
    if (!existsSync(walPath)) return [];

    const data = await readFile(walPath);
    const entries: Uint8Array[] = [];
    let offset = 0;

    while (offset < data.length) {
      if (offset + 4 > data.length) break;
      const lengthView = new Uint32Array(data.buffer.slice(data.byteOffset + offset, data.byteOffset + offset + 4));
      const entryLength = lengthView[0];
      offset += 4;

      if (offset + entryLength > data.length) break;
      entries.push(new Uint8Array(data.buffer.slice(data.byteOffset + offset, data.byteOffset + offset + entryLength)));
      offset += entryLength;
    }

    return entries;
  }
}
