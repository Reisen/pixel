import React, { useState } from 'react';
import styles              from './TagPanel.module.css';
import Tag                 from '../Tag';

interface Props {
    tags?: string[];
}

export default (props: Props) =>
    <div>
        <h1>Taglist</h1>
        <div className={styles.TagList}>
            {
                !props.tags ? null : props.tags.map(tag => (
                    <Tag tag={tag}/>
                ))
            }
            <Tag tag="yoshimitsu" category="artist" />
            <Tag tag="fighter" />
        </div>
    </div>
