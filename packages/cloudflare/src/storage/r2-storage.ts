/**
 * R2 storage adapter wrapping Cloudflare R2 (S3-compatible object storage).
 *
 * Provides the portable IBlobStorage interface from @agentic-primer/actors
 * around R2Bucket.
 * Follows brianln.ai patterns:
 * - Email Receiver: env.EMAIL_BUCKET.put(key, rawEmail)
 * - CMS: env.CONTENT.put(key, contentBody), env.ASSETS.put(key, mediaAsset)
 */

import type { IBlobStorage } from '@agentic-primer/actors';

/**
 * Cloudflare R2 adapter implementing IBlobStorage.
 *
 * Usage:
 * ```typescript
 * const blobs = new R2Storage(env.EMAIL_BUCKET);
 * await blobs.put(`emails/${id}/raw.eml`, rawEmailStream);
 * const email = await blobs.get(`emails/${id}/raw.eml`);
 * // email.body is ReadableStream
 * ```
 */
export class R2Storage implements IBlobStorage {
  constructor(private readonly bucket: R2Bucket) {}

  async get(
    key: string
  ): Promise<{
    body: ReadableStream;
    metadata?: Record<string, string>;
  } | null> {
    const object = await this.bucket.get(key);
    if (!object) return null;

    return {
      body: object.body,
      metadata: object.customMetadata,
    };
  }

  async put(
    key: string,
    body: ReadableStream | ArrayBuffer | string,
    options?: {
      httpMetadata?: Record<string, string>;
      customMetadata?: Record<string, string>;
    }
  ): Promise<void> {
    const putOptions: R2PutOptions = {};
    if (options?.httpMetadata) {
      putOptions.httpMetadata = options.httpMetadata as R2HTTPMetadata;
    }
    if (options?.customMetadata) {
      putOptions.customMetadata = options.customMetadata;
    }
    await this.bucket.put(key, body, putOptions);
  }

  async delete(key: string): Promise<void> {
    await this.bucket.delete(key);
  }

  async list(options?: {
    prefix?: string;
    limit?: number;
    cursor?: string;
  }): Promise<{
    objects: Array<{ key: string; size: number; uploaded: Date }>;
    cursor?: string;
  }> {
    const result = await this.bucket.list({
      prefix: options?.prefix,
      limit: options?.limit,
      cursor: options?.cursor,
    });

    return {
      objects: result.objects.map((obj) => ({
        key: obj.key,
        size: obj.size,
        uploaded: obj.uploaded,
      })),
      cursor: result.truncated ? result.cursor : undefined,
    };
  }

  async head(
    key: string
  ): Promise<{
    size: number;
    uploaded: Date;
    httpMetadata?: Record<string, string>;
  } | null> {
    const object = await this.bucket.head(key);
    if (!object) return null;

    // Convert R2HTTPMetadata to a plain Record<string, string>
    const httpMeta: Record<string, string> = {};
    if (object.httpMetadata) {
      const meta = object.httpMetadata;
      if (meta.contentType) httpMeta['contentType'] = meta.contentType;
      if (meta.contentLanguage)
        httpMeta['contentLanguage'] = meta.contentLanguage;
      if (meta.contentDisposition)
        httpMeta['contentDisposition'] = meta.contentDisposition;
      if (meta.contentEncoding)
        httpMeta['contentEncoding'] = meta.contentEncoding;
      if (meta.cacheControl) httpMeta['cacheControl'] = meta.cacheControl;
      if (meta.cacheExpiry)
        httpMeta['cacheExpiry'] = meta.cacheExpiry.toISOString();
    }

    return {
      size: object.size,
      uploaded: object.uploaded,
      httpMetadata:
        Object.keys(httpMeta).length > 0 ? httpMeta : undefined,
    };
  }
}
